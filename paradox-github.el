;;; paradox-github.el --- interacting with the Github API -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Prefix: paradox
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;


;;; Code:



;;; Github token
(defmacro paradox--enforce-github-token (&rest forms)
  "If a token is defined, perform FORMS, otherwise ignore forms ask for it be defined."
  `(if (stringp paradox-github-token)
       (progn ,@forms)
     (setq paradox-github-token nil)
     (paradox--check-github-token)))

(defun paradox--check-github-token ()
  "Check that the user has either set or refused the github token.
If neither has happened, ask the user now whether he'd like to
configure or refuse the token."
  (if (stringp paradox-github-token) t
    (if paradox-github-token
        t
      (if (not (y-or-n-p "Would you like to set up GitHub integration?
This will allow you to star/unstar packages from the Package Menu. "))
          (customize-save-variable 'paradox-github-token t)
        (describe-variable 'paradox-github-token)
        (when (get-buffer "*Help*")
          (switch-to-buffer "*Help*")
          (delete-other-windows))
        (if (y-or-n-p "Follow the instructions on the `paradox-github-token' variable.
May I take you to the token generation page? ")
            (browse-url "https://github.com/settings/tokens/new"))
        (message "Once you're finished, simply call `paradox-list-packages' again.")
        nil))))


;;; Starring
(defun paradox-star-all-installed-packages ()
  "Star all of your currently installed packages.
No questions asked."
  (interactive)
  (paradox--enforce-github-token
   (mapc (lambda (x) (paradox--star-package-safe (car-safe x))) package-alist)))

(defun paradox--star-package-safe (pkg &optional delete query)
  "Star PKG without throwing errors, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first."
  (let ((repo (cdr-safe (assoc pkg paradox--package-repo-list))))
    (when (and repo (not (assoc repo paradox--user-starred-list)))
      (paradox--star-repo repo delete query))))

(defun paradox--star-repo (repo &optional delete query)
  "Star REPO, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first.

Throws error if repo is malformed."
  (when (or (not query)
            (y-or-n-p (format "Really %sstar %s? "
                        (if delete "un" "") repo)))
    (paradox--github-action-star repo delete)
    (message "%starred %s." (if delete "Uns" "S") repo)
    (if delete
        (setq paradox--user-starred-list
              (remove repo paradox--user-starred-list))
      (add-to-list 'paradox--user-starred-list repo))))

(defun paradox--unstar-repo (repo &optional delete query)
  "Unstar REPO.
Calls (paradox--star-repo REPO (not DELETE) QUERY)."
  (paradox--star-repo repo (not delete) query))

(defun paradox--refresh-user-starred-list ()
  "Fetch the user's list of starred repos."
  (setq paradox--user-starred-list
        (ignore-errors
          (paradox--github-action
           "user/starred?per_page=100" nil
           'paradox--full-name-reader))))

(defun paradox--github-action-star (repo &optional delete no-result)
  "Call `paradox--github-action' with \"user/starred/REPO\" as the action.
DELETE and NO-RESULT are passed on."
  (paradox--github-action (concat "user/starred/" repo)
                          (if (stringp delete) delete (if delete "DELETE" "PUT"))
                          (null no-result)))


;;; The Base (generic) function
(defun paradox--github-action (action &optional method reader max-pages)
  "Contact the github api performing ACTION with METHOD.
Default METHOD is \"GET\".

Action can be anything such as \"user/starred?per_page=100\". If
it's not a full url, it will be prepended with
\"https://api.github.com/\".

The api action might not work if `paradox-github-token' isn't
set. This function also handles the pagination used in github
results, results of each page are appended. Use MAX-PAGES to
limit the number of pages that are fetched.

Return value is always a list.
- If READER is nil, the result of the action is completely
  ignored (no pagination is performed on this case, making it
  much faster).
- Otherwise:
  - If the result was a 404, the function returns nil;
  - Otherwise, READER is called as a function with point right
    after the headers and should always return a list. As a
    special exception, if READER is t, it is equivalent to a
    function that returns (t)."
  ;; Make sure the token's configured.
  (unless (string-match "\\`https?://" action)
    (setq action (concat "https://api.github.com/" action)))
  ;; Make the request
  (message "Contacting %s" action)
  (let ((pages (if (boundp 'pages) (1+ pages) 1)) next)
    (append
     (with-temp-buffer
       (save-excursion
         (shell-command
          (if (stringp paradox-github-token)
              (format "curl -s -i -d \"\" -X %s -u %s:x-oauth-basic \"%s\" "
                (or method "GET") paradox-github-token action)

            (format "curl -s -i -d \"\" -X %s \"%s\" "
              (or method "GET") action)) t))
       (when reader
         (unless (search-forward " " nil t)
           (message "%s" (buffer-string))
           (error ""))
         ;; 204 means OK, but no content.
         (if (looking-at "204") '(t)
           ;; 404 is not found.
           (if (looking-at "404") nil
             ;; Anything else gets interpreted.
             (when (search-forward-regexp "^Link: .*<\\([^>]+\\)>; rel=\"next\"" nil t)
               (setq next (match-string-no-properties 1)))
             (search-forward-regexp "^?$")
             (skip-chars-forward "[:blank:]\n")
             (delete-region (point-min) (point))
             (unless (eobp) (if (eq reader t) t (funcall reader)))))))
     (when (and next (or (null max-pages) (< pages max-pages)))
       (paradox--github-action next method reader)))))

(provide 'paradox-github)
;;; paradox-github.el ends here.
