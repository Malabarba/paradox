;;; paradox.el --- Display Package Ratings on the *Packages* buffer.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 0.5
;; Keywords: package packages mode-line
;; Package-Requires: ((emacs "24.1") (tabulated-list "1.0") (package "1.0"))
;; Prefix: paradox 
;; Separator: -

;;; Commentary:
;; 
;; Project for generating and displaying Package Ratings for Emacs packages.
;; 
;; To install it, open the file and call M-x `package-install-from-buffer'.
;; 
;; To use it, simply call M-x `paradox-list-packages' (instead of the regular `list-packages').
;; 
;; ## Current Features ##
;; 
;; * Display number of GitHub Stars the package has (right before the description).
;; * Display useful information on the mode-line and cleanup a bunch of
;;   useless stuff.
;; * `hl-line-mode' enabled by default.
;; * Customizable column widths, and automatic width for the `Archive' column.
;; 
;; ## Planned Features ##
;; 
;; * Star and unstar packages from within the Package Menu.
;; * Package filtering.
;; * More fontification.
;; * More customization.
;; 
;; ## Known Bugs ##
;; 
;; * On some paradox--cases there's an annoying gnutls error message after downloading the star counts
;;       <gnutls>.c: [0] (Emacs) fatal error: The TLS connection was non-properly terminated.
;;   If anyone knows how to fix it, I'm all ears.

;;; Instructions:
;;
;; INSTALLATION
;;
;; To install it, open the file and call M-x `package-install-from-buffer'.
;; 
;; To use it, simply call M-x `paradox-list-packages' (instead of the regular `list-packages').

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
;; 0.5 - 2014/04/13 - (Un)Star packages with the "s" key!.
;; 0.2 - 2014/04/13 - Control the face used for each status with paradox-status-face-alist.
;; 0.2 - 2014/04/13 - New archive face.
;; 0.2 - 2014/04/13 - Define filtering keys (fk, fu, fr).
;; 0.2 - 2014/04/11 - Hide buffer-name with paradox-display-buffer-name.
;; 0.2 - 2014/04/08 - Even better mode-line.
;; 0.2 - 2014/04/08 - Intelligent width for the "archive" column.
;; 0.2 - 2014/04/08 - Customizable widths.
;; 0.2 - 2014/04/08 - Prettier trunctation.
;; 0.1 - 2014/04/03 - Created File.
;;; Code:

(require 'package)
(require 'cl)
(defconst paradox-version "0.5" "Version of the paradox.el package.")
(defun paradox-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your emacs and paradox versions."
  (interactive)
  (message "Your paradox-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           paradox-version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/paradox/issues/new"))
(defun paradox-customize ()
  "Open the customization menu in the `paradox' group."
  (interactive)
  (customize-group 'paradox t))
(defgroup paradox nil
  "Customization group for paradox."
  :prefix "paradox-"
  :package-version '(paradox . "0.1"))

(defface paradox-name-face
  '((t :inherit link))
  "Face used on the name column."
  :group 'paradox)
;; (defface paradox-version-face
;;   '((t :inherit default))
;;   "Face used on the version column."
;;   :group 'paradox)
(defface paradox-archive-face
  '((((background light)) :foreground "Grey60")
    (((background dark)) :foreground "Grey40"))
  "Face used on the archive column."
  :group 'paradox)
(defface paradox-star-face
  '((t :inherit font-lock-string-face))
  "Face used on the star column."
  :group 'paradox)
;; (defface paradox-description-face
;;   '((t :inherit default))
;;   "Face used on the description column."
;;   :group 'paradox)

(defvar paradox--star-count nil)
(defvar paradox--package-repo-list nil)

(defvar paradox--star-count-url
  "https://raw.github.com/Bruce-Connor/paradox/data/data"
  "Address of the raw star-count file.")

(defmacro paradox--cas (string)
  `(cdr (assoc-string ,string paradox--package-count)))

(defadvice package-refresh-contents
    (before paradox-before-package-refresh-contents-advice () activate)
  "Download paradox data when updating packages buffer."
  (paradox--refresh-star-count))

;;;###autoload
(defun paradox--refresh-star-count ()
  "Download the star-count file and populate the respective variable."
  (interactive)
  (with-current-buffer 
      (url-retrieve-synchronously paradox--star-count-url)
    (setq paradox--star-count
          (when (search-forward "\n\n")
            (read (current-buffer))))
    (setq paradox--package-repo-list (read (current-buffer)))
    (kill-buffer)))

(defvar paradox-hide-buffer-identification t
  "If non-nil, no buffer-name will be displayed in the packages buffer.")
(defvaralias 'paradox-hide-buffer-name 'paradox-hide-buffer-identification)

(defun paradox--build-buffer-id (st n)
  (list st (list :propertize (int-to-string n)
                 'face 'mode-line-buffer-id)))

;;;###autoload
(defun paradox-list-packages (no-fetch)
  "Improved version of `package-list-packages'.
Shows star count for packages, and extra information in the
mode-line."
  (interactive "P")
  (paradox-enable)
  (unless no-fetch (paradox--refresh-star-count))
  (package-list-packages no-fetch))

(defun paradox-enable ()
  "Enable paradox, overriding the default package-menu."
  (interactive)
  (if (version< emacs-version "24.3.50")
      (paradox--override-definition 'package-menu--print-info 'paradox--print-info-compat)
    (paradox--override-definition 'package-menu--print-info 'paradox--print-info))
  (paradox--override-definition 'package-menu--generate 'paradox--generate-menu)
  (paradox--override-definition 'truncate-string-to-width 'paradox--truncate-string-to-width)
  (paradox--override-definition 'package-menu-mode 'paradox-menu-mode))

(defvar paradox--backups nil)

(defun paradox-disable ()
  "Disable paradox, and go back to regular package-menu."
  (interactive)
  (dolist (it paradox--backups)
    (message "Restoring %s to %s" (car it) (eval (cdr it)))
    (fset (car it) (eval (cdr it))))
  (setq paradox--backups nil))

(defun paradox--override-definition (sym newdef)
  "Temporarily override SYM's function definition with NEWDEF.
The original definition is saved to paradox--SYM-backup."
  (let ((backup-name (intern (format "paradox--%s-backup" sym)))
        (def (symbol-function sym)))
    (unless (assoc sym paradox--backups)
      (message "Overriding %s with %s" sym newdef)
      (eval (list 'defvar backup-name nil))
      (add-to-list 'paradox--backups (cons sym backup-name))
      (set backup-name def)
      (fset sym newdef))))

;;; Right now this is trivial, but we leave it as function so it's easy to improve.
(defun paradox--active-p ()
  (null (null paradox--backups)))

(defun paradox--truncate-string-to-width (&rest args)
  "Like `truncate-string-to-width', except default ellipsis is \"…\" on package buffer."
  (when (and (eq major-mode 'paradox-menu-mode)
             (eq t (nth 4 args)))
    (setf (nth 4 args) (if (char-displayable-p ?…) "…" "$")))
  (apply paradox--truncate-string-to-width-backup args))

(defvar paradox--upgradeable-packages nil)
(defvar paradox--upgradeable-packages-number nil)
(defvar paradox--upgradeable-packages-any? nil)

(defun paradox-refresh-upgradeable-packages ()
  "Refresh the list of upgradeable packages."
  (interactive)
  (setq paradox--upgradeable-packages (package-menu--find-upgrades))
  (setq paradox--upgradeable-packages-number
        (length paradox--upgradeable-packages))
  (setq paradox--upgradeable-packages-any?
        (> paradox--upgradeable-packages-number 0)))

(defcustom paradox-status-face-alist
  '(("built-in"  . font-lock-builtin-face)
    ("available" . default)
    ("new"       . bold)
    ("held"      . font-lock-constant-face)
    ("disabled"  . font-lock-warning-face)
    ("installed" . font-lock-comment-face)
    ("unsigned"  . font-lock-warning-face))
  "List of (\"STATUS\" . FACE) cons cells.
When displaying the package menu, FACE will be used to paint the
Version, Status, and Description columns of each package whose
status is STATUS. "
  :type '(repeat (cons string face))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defun paradox--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [STAR NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (car pkg))
         (status  (cdr pkg))
         (face (or (cdr (assoc-string status paradox-status-face-alist))
                   'font-lock-warning-face))) ; obsolete.
    (paradox--incf status)
    (list pkg-desc
          `[,(list (symbol-name (package-desc-name pkg-desc))
                   'face 'paradox-name-face
                   'follow-link t
                   'package-desc pkg-desc
                   'action 'package-menu-describe-package)
            ,(propertize (package-version-join
                          (package-desc-version pkg-desc))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg-desc) "")
                                    'font-lock-face 'paradox-archive-face)))
            ,(paradox--package-star-count (package-desc-name pkg-desc))
            ,(propertize (package-desc-summary pkg-desc)
                         'font-lock-face face)])))

(defun paradox--print-info-compat (pkg)
  "Return a package entry suitable for `tabulated-list-entries' (package-1.0 version).
PKG has the form ((PACKAGE . VERSION) STATUS DOC).
Return (KEY [NAME VERSION STATUS DOC]), where KEY is the
identifier (NAME . VERSION-LIST)."
  (let* ((package (caar pkg))
         (version (cdr (car pkg)))
         (status  (nth 1 pkg))
         (doc (or (nth 2 pkg) ""))
         (face (or (cdr (assoc-string status paradox-status-face-alist))
                   'font-lock-warning-face))) ; obsolete.
    (paradox--incf status)
    (list (cons package version)
          (vector (list (symbol-name package)
                        'face 'paradox-name-face
                        'follow-link t
                        'package-symbol package
                        'action 'package-menu-describe-package)
                  (propertize (package-version-join version)
                              'font-lock-face face)
                  (propertize status 'font-lock-face face)
                  (paradox--package-star-count package)
                  (propertize doc 'font-lock-face face)))))

(defun paradox--incf (status)
  (incf (paradox--cas status))
  (unless (string= status "obsolete")
    (incf (paradox--cas "total"))))

(defun paradox--improve-entry (entry)
  (setcdr entry (list 
                 (vconcat (list (paradox--entry-star-count entry))
                          (cadr entry)))))

(defun paradox--entry-star-count (entry)
  (paradox--package-star-count
   ;; The package symbol should be in the ID field, but that's not mandatory,
   (or (ignore-errors (elt (car entry) 1))
       ;; So we also try interning the package name.
       (intern (car (elt (cadr entry) 0))))))

(defun paradox--package-star-count (package)
  (propertize  
   (format "%s" (or (cdr (assoc package paradox--star-count)) ""))
   'face 'paradox-star-face))

(defun paradox--star-predicate (A B)
  (< (string-to-number (elt (cadr A) 4))
     (string-to-number (elt (cadr B) 4))))

(defvar paradox--current-filter nil)
(make-variable-buffer-local 'paradox--current-filter)

(defvar paradox--package-count
  '(("total" . 0) ("built-in" . 0)
    ("obsolete" . 0)
    ("available" . 0) ("new" . 0)
    ("held" . 0) ("disabled" . 0)
    ("installed" . 0) ("unsigned" . 0)))

(defun paradox--generate-menu (remember-pos packages &optional keywords)
  "Populate the Package Menu, without hacking into the header-format.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (mapc (lambda (x) (setf (cdr x) 0)) paradox--package-count)
  (package-menu--refresh packages keywords)
  (setq paradox--current-filter
        (if keywords (mapconcat 'identity keywords ",")
          nil))
  (let ((idx (paradox--column-index "^Package")))
    (setcar (aref tabulated-list-format idx)
            (if keywords
                (concat "Package[" paradox--current-filter "]")
              "Package")))  
  (if keywords
      (define-key package-menu-mode-map "q" 'package-show-package-list)
    (define-key package-menu-mode-map "q" 'quit-window))
  (tabulated-list-print remember-pos)
  (tabulated-list-init-header)
  (paradox--update-mode-line))

(defun paradox--column-index (regexp)
  (position regexp tabulated-list-format
            :test (lambda (x y) (string-match x (or (car-safe y) "")))))

(defvar paradox-menu-mode-map package-menu-mode-map)
(define-key paradox-menu-mode-map "f" 'paradox--filter-map)
(define-key paradox-menu-mode-map "s" 'paradox-menu-mark-star-unstar)

(define-prefix-command 'paradox--filter-map)
(define-key package-menu-mode-map "F" 'package-menu-filter)
(define-key paradox--filter-map "k" #'package-menu-filter)
(define-key paradox--filter-map "f" #'package-menu-filter)
(define-key paradox--filter-map "r" #'occur)
(define-key paradox--filter-map "o" #'occur)
(define-key paradox--filter-map "u" #'paradox-filter-upgrades)

(defun paradox-filter-upgrades ()
  "Show only upgradable packages."
  (interactive)
  (package-show-package-list
   (mapcar 'car paradox--upgradeable-packages)
   nil)
  (paradox--add-filter "Upgrade"))

(defun paradox--add-filter (keyword)
  "Append KEYWORD to `paradox--current-filter', and rebind \"q\"."
  ;; (unless (= 0 (length paradox--current-filter))
  ;;   (setq paradox--current-filter 
  ;;         (concat paradox--current-filter ",")))
  (setq paradox--current-filter keyword)
  (define-key package-menu-mode-map "q" 'quit-window))

(defcustom paradox-column-width-package  18
  "Width of the \"Package\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-version  9
  "Width of the \"Version\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-status  10
  "Width of the \"Status\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-column-width-star 4
  "Width of the \"Star\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(define-derived-mode paradox-menu-mode tabulated-list-mode "Paradox Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<paradox-menu-mode-map>
\\{paradox-menu-mode-map}"
  (hl-line-mode 1)  
  (paradox--update-mode-line)
  (setq tabulated-list-format
        `[("Package" ,paradox-column-width-package package-menu--name-predicate)
          ("Version" ,paradox-column-width-version nil)
          ("Status" ,paradox-column-width-status package-menu--status-predicate)
          ,@(paradox--archive-format)
          (,(if (char-displayable-p ?★) "★" "*") ,paradox-column-width-star paradox--star-predicate :right-align t)
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  ;; (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (add-hook 'tabulated-list-revert-hook 'paradox-refresh-upgradeable-packages nil t)
  (add-hook 'tabulated-list-revert-hook 'paradox--refresh-star-count nil t)
  (add-hook 'tabulated-list-revert-hook 'paradox--update-mode-line nil t)
  (tabulated-list-init-header)
  ;; We need package-menu-mode to be our parent, otherwise some
  ;; commands throw errors. But we can't actually derive from it,
  ;; otherwise its initialization will screw up the header-format. So
  ;; we "patch" it like this.
  (put 'paradox-menu-mode 'derived-mode-parent 'package-menu-mode)
  (run-hooks 'package-menu-mode-hook))

(defun paradox--archive-format ()
  (when (cdr package-archives)
    (list (list "Archive" 
                (apply 'max (mapcar 'length (mapcar 'car package-archives)))
                'package-menu--archive-predicate))))

(add-hook 'paradox-menu-mode-hook 'paradox-refresh-upgradeable-packages)

(defcustom paradox-local-variables
  '(mode-line-mule-info
    mode-line-client mode-line-modified
    mode-line-remote mode-line-position
    column-number-mode size-indication-mode
    (mode-line-front-space . " "))
  "Variables which will take special values on the Packages buffer.
This is a list, where each element is either SYMBOL or (SYMBOL . VALUE).

Each SYMBOL (if it is bound) will be locally set to VALUE (or
nil) on the Packages buffer."
  :type '(repeat (choice symbol (cons symbol sexp)))
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom paradox-display-buffer-name nil
  "If nil, *Packages* buffer name won't be displayed in the mode-line."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defun paradox--update-mode-line ()
  (mapc #'paradox--set-local-value paradox-local-variables)
  (setq mode-line-buffer-identification
        (list
         `(line-number-mode
           ("(" (:propertize "%4l" face mode-line-buffer-id) "/"
            ,(int-to-string (line-number-at-pos (point-max))) ")"))
         (list 'paradox-display-buffer-name
               (propertized-buffer-identification
                (format "%%%sb" (length (buffer-name)))))
         '(paradox--current-filter ("[" paradox--current-filter "]"))
         '(paradox--upgradeable-packages-any?
           (" " (:eval (paradox--build-buffer-id "Upgrade:" paradox--upgradeable-packages-number))))         
         '(package-menu--new-package-list
           (" " (:eval (paradox--build-buffer-id "New:" (paradox--cas "new")))))
         " " (paradox--build-buffer-id "Installed:" (+ (paradox--cas "installed") (paradox--cas "unsigned")))
         `(paradox--current-filter ""
                                   (" " ,(paradox--build-buffer-id "Total:" (length package-archive-contents)))))))

;;           (" " (:eval (paradox--build-buffer-id "New:" (length package-menu--new-package-list)))))
;;         " " (paradox--build-buffer-id "Installed:" (length package-alist))
;;         " " (paradox--build-buffer-id "Total:" (length package-archive-contents)))))

(defun paradox--set-local-value (x)
  (let ((sym (or (car-safe x) x)))
    (when (boundp sym)
      (set (make-local-variable sym) (cdr-safe x)))))


;;; Github api stuff
(defcustom paradox-github-token nil
  "Access token to use for github actions.
Currently, that means (un)starring repos.

To generate an access token:
  1. Visit the page https://github.com/settings/tokens/new
  2. Login to github (if asked).
  3. Give the token any name you want (Paradox, for instance).
  4. The only permission we need is \"public_repo\", so go ahead and unmark all others.
  5. Click on \"Generate Token\", copy the generated token, and save it to this variable."
  :type 'string
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defcustom paradox-automatically-star 'all
  "When you install new packages, should they be automatically
starred? Paradox is capable of automatically starring packages
when you install them. This variable defines whether this will
happen to all packages you install (recommended), only to
packages you manually request to install (by hitting \"I\"), or
not at all.

This variable must be a symbol, and has 4 possible values:
    all: Star ALL installed packages, including dependencies. (Default)
    installed-manually: Star installed packages, except those installed as dependencies.
    ask: Star ALL installed packages, but ask first (for each one).
    nil: Don't automatically star installed packages."
  :type '(choice (const :tag "Star installed packages, except those installed as dependencies." installed-manually)
                 (const :tag "Star ALL installed packages, including dependencies. (Default)" all)
                 (const :tag "Star ALL installed packages, but ask first (for each one)." ask)
                 (const :tag "Don't automatically star installed packages." nil))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defvar paradox--user-starred-list nil)

(defun paradox-menu-mark-star-unstar (n)
  "Mark a package for (un)starring and move to the next line."
  (interactive "p")
  (unless paradox--user-starred-list
    (paradox--refresh-user-starred-list))
  ;; Get package name
  (let ((pkg (intern (car (elt (tabulated-list-get-entry) 0))))
        will-delete)
    (unless pkg (error "Couldn't find package-name for this entry."))
    ;; get repo for this package
    (setq pkg (cdr-safe (assoc pkg paradox--package-repo-list)))
    ;; (Un)Star repo
    (if (not pkg)
        (message "This package is not a GitHub repo.")
      (setq will-delete (member pkg paradox--user-starred-list))
      (paradox--star-repo pkg will-delete))
    (forward-line 1)
    
    ;; (setq tag (if 
    ;;               "U" "S"))
    ;; (save-excursion
    ;;   (forward-line 0)
    ;;   (when (and (setq ctag (thing-at-point 'word))
    ;;              (string-match "\\`[ID]\\'" ctag))
    ;;     (setq tag (concat tag ctag))))
    ;; (tabulated-list-put-tag tag t)
    ))

(defun paradox--star-repo (pkg &optional delete query)
  (when (or (not query)
            (y-or-n-p (format "Really %sstar %s? "
                              (if delete "un" "") pkg)))  
    (paradox--github-action-star pkg delete)
    (message "%starred %s." (if delete "Uns" "S") pkg)
    (if delete
        (setq paradox--user-starred-list
              (remove pkg paradox--user-starred-list))
      (add-to-list 'paradox--user-starred-list pkg))))

(defun paradox--refresh-user-starred-list ()
  (setq paradox--user-starred-list
        (mapcar
         (lambda (x) (cdr (assoc 'full_name x)))
         (paradox--github-action "user/starred"))))

(defun paradox--github-action-star (repo &optional delete no-result)
  (paradox--github-action (concat "user/starred/" repo)
                          (if (stringp delete) delete (if delete "DELETE" "PUT"))
                          no-result))

(defun paradox--github-action (action &optional method no-result)
  ;; Make sure the token's configured.
  (unless (stringp paradox-github-token)
    (describe-variable 'paradox-github-token)
    (when (get-buffer "*Help*")
      (switch-to-buffer "*Help*")
      (delete-other-windows))
    (error "You need to set your access token first! See the `paradox-github-token' variable."))
  ;; Make the request
  (with-current-buffer (get-buffer-create "plplpl")
    (shell-command
     (format "curl -s -i -d \"\" -X %s -u %s:x-oauth-basic https://api.github.com/%s"
             (or method "GET") paradox-github-token action)
     t)
    (unless no-result
      (goto-char (point-min))
      (unless (search-forward "\nStatus: " nil t)
        (message "%s" (buffer-string))
        (error ""))
      ;; 204 means OK, but no content.
      (if (looking-at "204") t
        ;; 404 is not found.
        (if (looking-at "404") nil
          ;; Anything else gets interpreted.
          (search-forward-regexp "^?$")
          (skip-chars-forward "[:blank:]\n")
          (unless (eobp) (json-read)))))))

(provide 'paradox)
;;; paradox.el ends here.
