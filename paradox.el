;;; paradox.el --- Display Package Ratings on the *Packages* buffer.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 0.2
;; Keywords: 
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
;; 0.2 - 2014/04/11 - Hide buffer-name with paradox-display-buffer-name.
;; 0.2 - 2014/04/08 - Even better mode-line.
;; 0.2 - 2014/04/08 - Intelligent width for the "archive" column.
;; 0.2 - 2014/04/08 - Customizable widths.
;; 0.2 - 2014/04/08 - Prettier trunctation.
;; 0.1 - 2014/04/03 - Created File.
;;; Code:

(require 'package)
(defconst paradox-version "0.2" "Version of the paradox.el package.")
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

(defface paradox-star-face
  '((t :inherit font-lock-comment-face))
  "Face used on the star count number."
  :group 'paradox)

(defvar paradox--star-count nil)

(defvar paradox--star-count-url
  "https://raw.github.com/Bruce-Connor/paradox/data/star-count"
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
  (setq
   paradox--star-count
   (with-current-buffer 
       (url-retrieve-synchronously paradox--star-count-url)
     (when (search-forward "\n\n")
       (read (current-buffer))))))

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

(defun paradox--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [STAR NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (car pkg))
         (status  (cdr pkg))
         (face (pcase status
                 (`"built-in"  'font-lock-builtin-face)
                 (`"available" 'default)
                 (`"new"       'bold)
                 (`"held"      'font-lock-constant-face)
                 (`"disabled"  'font-lock-warning-face)
                 (`"installed" 'font-lock-comment-face)
                 (`"unsigned"  'font-lock-warning-face)
                 (_            'font-lock-warning-face)))) ; obsolete.
    (paradox--incf status)
    (list pkg-desc
          `[,(list (symbol-name (package-desc-name pkg-desc))
                   'face 'link
                   'follow-link t
                   'package-desc pkg-desc
                   'action 'package-menu-describe-package)
            ,(propertize (package-version-join
                          (package-desc-version pkg-desc))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg-desc) "")
                                    'font-lock-face face)))
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
         (face (cond
                ((string= status "built-in")  'font-lock-builtin-face)
                ((string= status "available") 'default)
                ((string= status "new") 'bold)
                ((string= status "held")      'font-lock-constant-face)
                ((string= status "disabled")  'font-lock-warning-face)
                ((string= status "installed") 'font-lock-comment-face)
                (t 'font-lock-warning-face)))) ; obsolete.
    
    (paradox--incf status)
    (list (cons package version)
          (vector (list (symbol-name package)
                        'face 'link
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
  (incf (paradox--cas "total")))

(defun paradox--improve-entry (entry)
  (setcdr entry (list 
                 (vconcat (list (paradox--entry-star-count entry))
                          (cadr entry)))))

(defun paradox--entry-star-count (entry)
  (paradox--package-star-count ;; The package symbol should be in the ID field, but that's not mandatory,
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

(provide 'paradox)
;;; paradox.el ends here.
