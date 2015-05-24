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

;;; Code:


(require 'paradox-github)
(require 'json)
(eval-when-compile (require 'cl))

(defcustom paradox-download-count-url
  "http://melpa.org/download_counts.json"
  ""
  :type 'string
  :group 'paradox)

(defvar paradox--output-data-file-old
  (expand-file-name "../data")
  "File where lists will be saved.")

(defcustom paradox--output-data-file
  (expand-file-name "../data-hashtables")
  "File where hashtables will be saved."
  :type 'file
  :group 'paradox-counter
  :package-version '(paradox-counter . "0.1"))

(defun paradox-log (&rest s)
  (princ (concat (apply #'format s) "\n") t))

(defun paradox--alist-to-table (list)
  (let ((table (make-hash-table)))
    (dolist (it list)
      (if (consp it)
          (puthash (car it) (cdr it) table)
        (puthash it t table)))
    table))

(defun paradox-list-to-file ()
  "Save lists in \"data\" file."
  (with-temp-file paradox--output-data-file-old
    (pp paradox--star-count (current-buffer))
    (pp paradox--package-repo-list (current-buffer))
    (pp paradox--download-count (current-buffer))
    (pp paradox--wiki-packages (current-buffer)))
  (with-temp-file paradox--output-data-file
    (pp (paradox--alist-to-table paradox--star-count) (current-buffer))
    (pp (paradox--alist-to-table paradox--package-repo-list) (current-buffer))
    (pp (paradox--alist-to-table paradox--download-count) (current-buffer))
    (pp (paradox--alist-to-table paradox--wiki-packages) (current-buffer))))

(defun paradox-fetch-star-count (repo)
  (cdr (assq 'stargazers_count
             (paradox--github-action (format "repos/%s" repo)
                                     :reader #'json-read))))


;;;###autoload
(defun paradox-generate-star-count (&optional _recipes-dir)
  "Get the number of stars for each github repo and return.
Also saves result to `package-star-count'"
  (interactive)
  (setq paradox-github-token
        (or (getenv "GHTOKEN") paradox-github-token))
  (require 'json)
  ;; First, generate the list file. Remove this once the hash tables
  ;; are on a stable release.
  (setq paradox--star-count nil)
  (setq paradox--package-repo-list nil)
  (setq paradox--wiki-packages nil)
  (let ((json-key-type 'symbol)
        (json-object-type 'alist))
    (setq paradox--download-count
          (paradox--github-action paradox-download-count-url :reader #'json-read)))
  (with-current-buffer (url-retrieve-synchronously "http://melpa.org/recipes.json")
    (search-forward "\n\n")
    (let ((i 0))
      (dolist (it (json-read))
        (let ((name (car it)))
          (let-alist (cdr it)
            (paradox-log "%s / %s" (incf i) name)
            (pcase .fetcher
              (`"github"
               (let ((count (paradox-fetch-star-count .repo)))
                 (when (numberp count)
                   (push (cons name count) paradox--star-count)
                   (push (cons name .repo) paradox--package-repo-list))))
              (`"wiki"
               (push name paradox--wiki-packages))))))))
  (paradox-list-to-file))

(provide 'paradox-counter)
;;; paradox-counter.el ends here.
