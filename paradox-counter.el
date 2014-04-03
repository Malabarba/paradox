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
    (setq recipes-dir package-recipes-directory))
  (setq paradox-star-count nil)
  (with-temp-buffer
    (dolist (file (directory-files recipes-dir t))
      (unless (string-match "/\\.dir-locals\\.el\\'" file)
        (insert-file-contents file)
        (let ((package (read (buffer-string)))
              (sc 0)
              owner/repo)
          (when (eq 'github (cadr (memq :fetcher package)))
            (setq owner/repo (cadr (memq :repo package)))
            (with-current-buffer 
                (url-retrieve-synchronously 
                 (format "https://api.github.com/repos/%s/stargazers" owner/repo))
              (search-forward-regexp "^$" nil t)
              (forward-char 1)
              (if (looking-at "[")
                  (while (null (looking-at "]"))
                    (incf sc)
                    (forward-sexp 1)))
              (kill-buffer))
            (add-to-list 'package-star-count
                         (cons (car package) sc))))))
    (erase-buffer))
  (paradox-list-to-file 'paradox-star-count))

(defun paradox-list-to-file (name)
  "Save list NAME in file given by the NAME-output-file variable."
  (let ((filename (eval (intern (format "%s-output-file" name)))))
    (with-temp-file filename
      (princ (eval name) (current-buffer)))))

(provide 'paradox-counter)
;;; paradox-counter.el ends here.
