;;; paradox.el --- Display Package Ratings on the *Packages* buffer.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 0.1
;; Keywords: 
;; Prefix: paradox 
;; Separator: -

;;; Commentary:
;;
;; 

;;; Instructions:
;;
;; INSTALLATION
;;
;; This package is available fom Melpa, you may install it by calling
;; M-x package-install.
;;
;; Alternatively, you can download it manually, place it in your
;; `load-path' and require it with
;;
;;     (require 'paradox)

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

(defconst paradox-version "0.1" "Version of the paradox.el package.")
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

(defvar paradox-star-count nil)

(defcustom paradox-star-count-url
  "https://raw.github.com/Bruce-Connor/paradox/data/star-count"
  "Address of the raw star-count file."
  :type 'url
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defadvice package-refresh-contents
    (before paradox-before-package-refresh-contents-advice () activate)
  "Download paradox data when updating packages buffer."
  (paradox-refresh-star-count))

;;;###autoload
(defun paradox-refresh-star-count ()
  "Download the star-count file and populate the respective variable."
  (interactive)
  (setq
   paradox-star-count
   (with-current-buffer 
       (url-retrieve-synchronously paradox-star-count-url)
     (when (search-forward "\n\n")
       (read (current-buffer))))))

(defvar paradox-hide-buffer-identification t
  "If non-nil, no buffer-name will be displayed in the packages buffer.")
(defvaralias 'paradox-hide-buffer-name 'paradox-hide-buffer-identification)

(defvar paradox-installed-count
  (concat 
   "Installed: "
   (propertize "30" 'face 'mode-line-buffer-id)))
(put 'paradox-installed-count 'risky-local-variable t)

(defvar paradox-total-count "30")
(put 'paradox-total-count 'risky-local-variable t)

(define-derived-mode paradox-menu-mode tabulated-list-mode "Paradox Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<package-menu-mode-map>
\\{package-menu-mode-map}"
  (setq mode-line-buffer-identification
        (list
         (propertized-buffer-identification
          (format "%%%sb" (length (buffer-name))))
         " " 
         '(:eval (propertize paradox-installed-count
                             'mouse-face 'mode-line-highlight
                             'local-map mode-line-buffer-identification-keymap))
         " " 
         '(:propertize ("Total: " (:propertize paradox-total-count face mode-line-buffer-id))
                       mouse-face mode-line-highlight)))
  (setq tabulated-list-format
        `[("Package" 18 package-menu--name-predicate)
          ("Version" 12 nil)
          ("Status"  10 package-menu--status-predicate)
          ,@(if (cdr package-archives)
                '(("Archive" 10 package-menu--archive-predicate)))
          ("Description" 0 nil)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (tabulated-list-init-header))

(provide 'paradox)
;;; paradox.el ends here.
