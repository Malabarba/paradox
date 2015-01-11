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



;;; Internal variables
(defvar paradox--star-count nil)
(defvar paradox--download-count nil)
(defvar paradox--package-repo-list nil)

(defvar paradox--star-count-url
  "https://raw.githubusercontent.com/Bruce-Connor/paradox/data/data"
  "Address of the raw star-count file.")

(defvar paradox--package-count
  '(("total" . 0) ("built-in" . 0)
    ("obsolete" . 0) ("deleted" . 0)
    ("available" . 0) ("new" . 0)
    ("held" . 0) ("disabled" . 0)
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


(provide 'paradox-core)
;;; paradox-core.el ends here.
