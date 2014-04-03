;;; paradox-counter.el --- Functions for counting the number of stars on each github emacs package.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 0.1
;; Prefix: paradox
;; Separator: -
;; Requires: paradox

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

;;; Change Log:
;; 0.1 - 2014/04/03 - Generator complete.
;; 0.1 - 2014/04/03 - Created File.
;;; Code:

(require 'paradox)
(defconst paradox-counter-version "0.1" "Version of the paradox-counter.el package.")

(defcustom paradox-melpa-directory
  (expand-file-name "~/Git-Projects/melpa/")
  "Directory with melpa package recipes."
  :type 'directory
  :group 'paradox)

(defcustom paradox-recipes-directory
  (when (file-directory-p (concat paradox-melpa-directory "recipes/"))
    (concat paradox-melpa-directory "recipes/"))
  "Directory with melpa package recipes."
  :type 'directory
  :group 'paradox)

(defcustom paradox-star-count-output-file 
  (expand-file-name "./star-count")
  "File where a list of star counts will be saved."
  :type 'file
  :group 'paradox-counter
  :package-version '(paradox-counter . "0.1"))

;;;###autoload
(defun paradox-generate-star-count (&optional recipes-dir)
  "Get the number of stars for each github repo and return.
Also saves result to `package-star-count'"
  (interactive)
  (unless recipes-dir
    (setq recipes-dir paradox-recipes-directory))
  (setq paradox-star-count nil)
  (with-temp-buffer
    (let* ((i 0)
           (files (directory-files recipes-dir t "\\`[^\\.]"))
           (N (length files)))
      (dolist (file files)
        (message "%s / %s" (incf i) N)
        (insert-file-contents file)
        (let ((package (read (buffer-string))))
          (when (eq 'github (cadr (memq :fetcher package)))
            (add-to-list
             'paradox-star-count
             (cons (car package)
                   (paradox-fetch-star-count (cadr (memq :repo package)))))))
        (erase-buffer))))
  (paradox-list-to-file 'paradox-star-count))

(defun paradox-log (&rest s)
  (apply 'message s))

(defun paradox-list-to-file (name)
  "Save list NAME in file given by the NAME-output-file variable."
  (let ((filename (eval (intern (format "%s-output-file" name)))))
    (with-temp-file filename
      (pp (eval name) (current-buffer)))))

(defun paradox-github-api-request (req)
  (with-temp-buffer
    (shell-command
     (format 
      "curl -s -u %s:x-oauth-basic https://api.github.com/%s"
      paradox-personal-access-token req)
     t)
    (goto-char (point-min))
    (json-read)))

(defun paradox-fetch-star-count (repo)
  (with-current-buffer
      (url-retrieve-synchronously (format "https://github.com/%s/" repo))
    (when (search-forward-regexp (format "href=\"/%s/stargazers\">" repo) nil t)
      (skip-chars-forward "\n 	")
      (if (looking-at "[0-9]")
          (thing-at-point 'number)
        nil))))

(provide 'paradox-counter)
;;; paradox-counter.el ends here.
