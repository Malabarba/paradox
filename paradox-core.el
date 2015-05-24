;;; paradox-core.el --- common functions -*- lexical-binding:t -*-

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
(require 'subr-x)


;;; Configurations
(defface paradox-comment-face
  '((((background light)) :foreground "Grey30")
    (((background dark)) :foreground "Grey60"))
  "Face used on faded out stuff."
  :group 'paradox-menu
  :group 'paradox-commit-list)
(defface paradox-highlight-face
  '((t :weight bold :inherit font-lock-variable-name-face))
  "Face used on highlighted stuff."
  :group 'paradox-menu
  :group 'paradox-commit-list)


;;; Internal variables
(defvar paradox--star-count (make-hash-table))
(defvar paradox--download-count (make-hash-table))
(defvar paradox--package-repo-list (make-hash-table))
(defvar paradox--wiki-packages (make-hash-table))

(defconst paradox--data-url
  "https://raw.githubusercontent.com/Malabarba/paradox/data/"
  "Address of Paradox's data directory.")

(defconst paradox--star-count-url (concat paradox--data-url "data-hashtables")
  "Address of the raw star-count file.")
(make-obsolete-variable 'paradox--star-count-url 'paradox--data-url "2.1")

(defconst paradox--package-count
  '(("total" . 0) ("built-in" . 0)
    ("obsolete" . 0) ("deleted" . 0)
    ("available" . 0) ("new" . 0)
    ("held" . 0) ("disabled" . 0)
    ("dependency" . 0) ("avail-obso" . 0)
    ("incompat" . 0)  ("external" . 0)
    ("installed" . 0) ("unsigned" . 0)))

(defvar paradox--truncate-string-to-width-backup)

(defmacro paradox--cas (string)
  "Same as (cdr (assoc-string ,STRING paradox--package-count))."
  `(cdr (assoc-string ,string paradox--package-count)))

(defun paradox--truncate-string-to-width (&rest args)
  "Like `truncate-string-to-width', but uses \"…\" on package buffer.
All arguments STR, END-COLUMN, START-COLUMN, PADDING, and
ELLIPSIS are passed to `truncate-string-to-width'.

\(fn STR END-COLUMN &optional START-COLUMN PADDING ELLIPSIS)"
  (when (and (eq major-mode 'paradox-menu-mode)
             (eq t (nth 4 args)))
    (setf (nth 4 args) (if (char-displayable-p ?…) "…" "$")))
  (apply paradox--truncate-string-to-width-backup args))


;;; Overriding definitions
(defun paradox--core-enable ()
  "Enable core features."
  (paradox--override-definition 'truncate-string-to-width 'paradox--truncate-string-to-width))

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


;;; Pre 25.1 support
(declare-function paradox--update-downloads-in-progress "paradox-menu")
(if (fboundp 'package--update-downloads-in-progress)
    (defun paradox--update-downloads-in-progress (&optional name)
      (when name
        (package--update-downloads-in-progress name)))
  (defalias 'paradox--update-downloads-in-progress #'ignore))
(define-obsolete-function-alias
  'paradox--pdate-downloads-in-progress
  'paradox--update-downloads-in-progress
  "2.1")


;;; Spinner
(defvar paradox--spinner nil)

(eval-and-compile (require 'spinner))
(defcustom paradox-spinner-type 'horizontal-moving
  "Holds the type of spinner to be used in the mode-line.
Takes a value accepted by `spinner-start'."
  :type `(choice (choice :tag "Choose a spinner by name"
                         ,@(mapcar (lambda (c) (list 'const (car c)))
                                   spinner-types))
                 (const :tag "A random spinner" random)
                 (repeat :tag "A list of symbols from `spinner-types' to randomly choose from"
                         (choice :tag "Choose a spinner by name"
                                 ,@(mapcar (lambda (c) (list 'const (car c)))
                                           spinner-types)))
                 (vector :tag "A user defined vector"
                         (repeat :inline t string)))
  :package-version '(paradox . "2.1")
  :group 'paradox-execute)

(defun paradox--start-spinner ()
  (when (spinner-p paradox--spinner)
    (spinner-stop paradox--spinner))
  (setq paradox--spinner
        (make-spinner paradox-spinner-type t 10))
  (spinner-start paradox--spinner))

(defun paradox--stop-spinner ()
  (when (spinner-p paradox--spinner)
    (spinner-stop paradox--spinner))
  (setq paradox--spinner nil))

(provide 'paradox-core)
;;; paradox-core.el ends here.
