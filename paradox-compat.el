;;; paradox-compat.el --- Compatibility functions for using paradox with emacs < 24.4

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 0.5
;; Keywords: package packages mode-line
;; Package-Requires: ((emacs "24.1") (tabulated-list "1.0") (package "1.0") (json "1.4"))
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

(eval-when-compile (require 'package))
;; (require 'json)
;; (require 'cl)


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

(defmacro package--push-compat (package desc status listname)
  "Convenience macro for `package-menu--generate'.
If the alist stored in the symbol LISTNAME lacks an entry for a
package PACKAGE with descriptor DESC, add one.  The alist is
keyed with cons cells (PACKAGE . VERSION-LIST), where PACKAGE is
a symbol and VERSION-LIST is a version list."
  `(let* ((version (package-desc-vers ,desc))
          (key (cons ,package version)))
     (unless (assoc key ,listname)
       (push (list key ,status (package-desc-doc ,desc)) ,listname))))

(defun paradox-menu--refresh (packages &optional keywords)
  ;; Construct list of ((PACKAGE . VERSION) STATUS DESCRIPTION).
  (let (info-list name)
    ;; Installed packages:
    (dolist (elt package-alist)
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (package--push-compat name (cdr elt)
                       (if (stringp (cadr (assq name package-load-list)))
                           "held" "installed")
                       info-list)))

    ;; Built-in packages:
    (dolist (elt package--builtins)
      (setq name (car elt))
      (when (and (not (eq name 'emacs)) ; Hide the `emacs' package.
                 (or (eq packages t) (memq name packages)))
        (package--push-compat name (cdr elt) "built-in" info-list)))

    ;; Available and disabled packages:
    (dolist (elt package-archive-contents)
      (setq name (car elt))
      (when (or (eq packages t) (memq name packages))
        (let ((hold (assq name package-load-list)))
          (package--push-compat name (cdr elt)
                         (cond
                          ((and hold (null (cadr hold))) "disabled")
                          ((memq name package-menu--new-package-list) "new")
                          (t "available"))
                         info-list))))

    ;; Obsolete packages:
    (dolist (elt package-obsolete-alist)
      (dolist (inner-elt (cdr elt))
        (when (or (eq packages t) (memq (car elt) packages))
          (package--push-compat (car elt) (cdr inner-elt) "obsolete" info-list))))

    ;; Print the result.
    (setq tabulated-list-entries (mapcar 'package-menu--print-info info-list))
    (tabulated-list-print remember-pos)))

(provide 'paradox-compat)
;;; paradox-compat.el ends here.
