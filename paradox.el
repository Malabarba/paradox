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

;;;###autoload


(provide 'paradox)
;;; paradox.el ends here.
