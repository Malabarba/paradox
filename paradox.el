;;; paradox.el --- A modern Packages Menu. Colored, with package ratings, and customizable. -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 2.0
;; Keywords: package packages mode-line
;; Package-Requires: ((emacs "24.4") (dash "2.6.0") (cl-lib "0.5") (json "1.3") (let-alist "1.0.3"))
;; Prefix: paradox
;; Separator: -

;;; Commentary:
;;
;; Paradox can be installed from Melpa with M-x `package-install' RET
;; paradox.
;; It can also be installed manually in the usual way, just be mindful of
;; the dependencies.
;;
;; To use it, simply call M-x `paradox-list-packages' (instead of the
;; regular `list-packages').
;; This will give you most features out of the box. If you want to be
;; able to star packages as well, just configure the
;; `paradox-github-token' variable then call `paradox-list-packages'
;; again.
;;
;; If you'd like to stop using Paradox, you may call `paradox-disable'
;; and go back to using the regular `list-packages'.
;;
;; ## Current Features ##
;;
;; ### Several Improvements ###
;;
;; Paradox implements many small improvements to the package menu
;; itself. They all work out of the box and are completely customizable!
;; *(Also, hit `h' to see all keys.)*
;;
;; * Visit the package's homepage with `v' (or just use the provided buttons).
;; * Shortcuts for package filtering:
;;     * <f r> filters by regexp (`occur');
;;     * <f u> display only packages with upgrades;
;;     * <f k> filters by keyword (Emacs 24.4 only).
;; * `hl-line-mode' enabled by default.
;; * Display useful information on the mode-line and cleanup a bunch of
;;   useless stuff.
;; * **Customization!** Just call M-x `paradox-customize' to see what you can
;;   do.
;;     * Customize column widths.
;;     * Customize faces (`paradox-star-face', `paradox-status-face-alist' and `paradox-archive-face').
;;     * Customize local variables.
;;
;; ### Package Ratings ###
;;
;; Paradox also integrates with
;; **GitHub Stars**, which works as **rough** package rating system.
;; That is, Paradox package menu will:
;;
;; 1. Display the number of GitHub Stars each package has (assuming it's
;;    in a github repo, of course);
;; 2. Possibly automatically star packages you install, and unstar
;;    packages you delete (you will be asked the first time whether you
;;    want this);
;; 3. Let you star and unstar packages by hitting the `s' key;
;; 4. Let you star all packages you have installed with M-x `paradox-star-all-installed-packages'.
;;
;; Item **1.** will work out of the box, the other items obviously
;; require a github account (Paradox will help you generate a token the
;; first time you call `paradox-list-packages').
;;
;; ## How Star Displaying Works ##
;;
;; We generate a map of Package Name -> Repository from
;; [Melpa](https://github.com/milkypostman/melpa.git)'s `recipe'
;; directory, some repos may correspond to more than one package.
;; This map is used count the stars a given package has.
;; _This doesn't mean you need Melpa to see the star counts, the numbers
;; will be displayed regardless of what archives you use._
;;
;; Currently, packages that are not hosted on GitHub are listed with a
;; blank star count, which is clearly different from 0-star packages
;; (which are displayed with a 0, obviously).
;; If you know of an alternative that could be used for these packages,
;; [open an issue](https://github.com/Bruce-Connor/paradox/issues/new)
;; here, I'd love to hear.

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
;; 2.0   - 2015/01/10 - New command `paradox-filter-clear'. Bound to `f c'.
;; 2.0   - 2015/01/10 - New hook `paradox-after-execute-functions'.
;; 2.0   - 2015/01/05 - Drop 24.3 support.
;; 2.0   - 2014/12/25 - `paradox-upgrade-packages' upgrades everything without question.
;; 2.0   - 2014/12/13 - `paradox-menu-execute' can do asynchronous (background) operations.
;; 1.2   - 2014/05/15 - Integration with smart-mode-line.
;; 1.1   - 2014/07/02 - NEW FUNCTION: paradox-require.
;; 1.1   - 2014/05/10 - Added Download column.
;; 1.0.2 - 2014/05/09 - Small improvements to paradox--github-action.
;; 1.0.1 - 2014/05/09 - Fix weird corner case in --package-homepage.
;; 1.0   - 2014/05/05 - New Feature! The l key displays a list of recent commits under a package.
;; 1.0   - 2014/05/04 - q key is smarter. It closes other generated windows.
;; 1.0   - 2014/05/04 - j and k describe the next and previous entries.
;; 0.11  - 2014/05/01 - Sorting commands and keys (under "S").
;; 0.10  - 2014/04/26 - New help menu!
;; 0.10  - 2014/04/25 - Display description on a separate line with paradox-lines-per-entry.
;; 0.10  - 2014/04/25 - Links to package homepages.
;; 0.9.2 - 2014/04/15 - Fix advice being enabled automatically.
;; 0.9.2 - 2014/04/15 - Ask the user before automatically starring.
;; 0.9.1 - 2014/04/14 - paradox-filter-upgrades is informative when there are no upgrades.
;; 0.9   - 2014/04/14 - First full feature release.
;; 0.5   - 2014/04/14 - Star all installed packages.
;; 0.5   - 2014/04/13 - (Un)Star packages with the "s" key!.
;; 0.2   - 2014/04/13 - Control the face used for each status with paradox-status-face-alist.
;; 0.2   - 2014/04/13 - New archive face.
;; 0.2   - 2014/04/13 - Define filtering keys (fk, fu, fr).
;; 0.2   - 2014/04/11 - Hide buffer-name with paradox-display-buffer-name.
;; 0.2   - 2014/04/08 - Even better mode-line.
;; 0.2   - 2014/04/08 - Intelligent width for the "archive" column.
;; 0.2   - 2014/04/08 - Customizable widths.
;; 0.2   - 2014/04/08 - Prettier trunctation.
;; 0.1   - 2014/04/03 - Created File.

;;; Code:

(require 'package)
(require 'cl-lib)
(require 'dash)
(defconst paradox-version "2.0" "Version of the paradox.el package.")
(defun paradox-bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your Emacs and paradox versions."
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
  :group 'emacs
  :package-version '(paradox . "0.1"))


;;; Customization Variables
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

(defvar paradox--column-name-star
  (if (char-displayable-p ?★) "★" "*"))

(defcustom paradox-column-width-download 4
  "Width of the \"Download Count\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "1.1"))

(defvar paradox--column-name-download
  (if (char-displayable-p ?↓) "↓" "DC"))

(defcustom paradox-github-token nil
  "Access token to use for github actions.
Currently, that means (un)starring repos.

To generate an access token:
  1. Visit the page https://github.com/settings/tokens/new and
     login to github (if asked).
  2. Give the token any name you want (Paradox, for instance).
  3. The only permission we need is \"public_repo\", so unmark
     all others.
  4. Click on \"Generate Token\", copy the generated token, and
     save it to this variable by writing
         (setq paradox-github-token TOKEN)
     somewhere in your configuration and evaluating it (or just
     restart emacs).

This is similar to how erc or jabber handle authentication in
emacs, but the following disclaimer always worth reminding.

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account.
END DISCLAIMER

Paradox will ask you whether you want github integration the
first time you start it. If you answer \"no\", it will remember
your choice via `customize-save-variable'. You can do this
manually by setting this variable to t. Setting it to nil means
it hasn't been configured yet."
  :type '(choice (string :tag "Token")
                 (const :tag "Disable" t)
                 (const :tag "Ask me next time" nil))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defcustom paradox-automatically-star 'unconfigured
  "When you install new packages, should they be automatically starred?
This variable has no effect if `paradox-github-token' isn't set
to a string.

Paradox is capable of automatically starring packages when you
install them, and unstarring when you delete them. This only
applies to actual installation/deletion, i.e. Paradox doesn't
auto (un)star packages that were simply upgraded.

If this variable is nil, this behaviour is disabled. \\<paradox-menu-mode-map>

On the Package Menu, you can always manually star packages with \\[paradox-menu-mark-star-unstar]."
  :type '(choice (const :tag "Yes." t)
                 (const :tag "No." nil)
                 (const :tag "Ask later." unconfigured))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defcustom paradox-display-star-count t
  "If non-nil, adds a \"Star\" column to the Package Menu."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "1.1"))

(defcustom paradox-display-download-count nil
  "If non-nil, adds a \"Download\" column to the Package Menu."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "1.2.3"))

(defface paradox-mode-line-face
  '((t :inherit mode-line-buffer-id :weight normal :foreground "Black"))
  "Face used on the package's name."
  :group 'paradox)
(defface paradox-name-face
  '((t :inherit link))
  "Face used on the package's name."
  :group 'paradox)
(defface paradox-homepage-button-face
  '((t :underline t :inherit font-lock-comment-face))
  "Face used on the homepage button."
  :group 'paradox)
;; (defface paradox-version-face
;;   '((t :inherit default))
;;   "Face used on the version column."
;;   :group 'paradox)
(defface paradox-archive-face
  '((t :inherit paradox-comment-face))
  "Face used on the archive column."
  :group 'paradox)
(defface paradox-star-face
  '((t :inherit font-lock-string-face))
  "Face used on the star column, for packages you haven't starred."
  :group 'paradox)
(defface paradox-starred-face
  '((t :weight bold :inherit paradox-star-face))
  "Face used on the star column, for packages you have starred."
  :group 'paradox)
(defface paradox-download-face
  '((t :inherit font-lock-keyword-face))
  "Face used on the Downloads column."
  :group 'paradox)
(defface paradox-description-face
  '((t :inherit default))
  "Face used on the description column.
If `paradox-lines-per-entry' > 1, the face
`paradox-description-face-multiline' is used instead."
  :group 'paradox)
(defface paradox-description-face-multiline
  '((t :inherit font-lock-doc-face))
  "Face used on the description column when `paradox-lines-per-entry' > 1.
If `paradox-lines-per-entry' = 1, the face
`paradox-description-face' is used instead."
  :group 'paradox)

(defface paradox-comment-face
  '((((background light)) :foreground "Grey30")
    (((background dark)) :foreground "Grey60"))
  "Face used on faded out stuff."
  :group 'paradox)
(defface paradox-highlight-face
  '((t :weight bold :inherit font-lock-variable-name-face))
  "Face used on highlighted stuff."
  :group 'paradox)

(defface paradox-commit-tag-face
  '((t :foreground "goldenrod4"
       :background "LemonChiffon1"
       :box 1))
  "Face used for tags on the commit list."
  :group 'paradox-commit-list)

(defcustom paradox-execute-asynchronously 'ask
  "Whether the install/delete/upgrade should be asynchronous.
Possible values are:
  t, which means always;
  nil, which means never;
  ask, which means ask each time."
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Ask each time" ask))
  :package-version '(paradox . "2.0"))


;;; Internal Variables
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

(defvar paradox--current-filter nil)
(make-variable-buffer-local 'paradox--current-filter)

(defvar paradox--commit-list-buffer "*Package Commit List*")

(defvar paradox--truncate-string-to-width-backup)

(defmacro paradox--cas (string)
  "Same as (cdr (assoc-string ,STRING paradox--package-count))."
  `(cdr (assoc-string ,string paradox--package-count)))

(defvar paradox--data-url "https://raw.github.com/Bruce-Connor/paradox/data/full"
  "Address of the raw data file.")


;;; Mode Definition
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
          ,@(paradox--count-format)
          ("Description" 0 nil)])
  (setq paradox--column-index-star
        (paradox--column-index paradox--column-name-star))
  (setq paradox--column-index-download
        (paradox--column-index paradox--column-name-download))
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

(defun paradox--define-sort (name &optional key)
  "Define sorting function paradox-sort-by-NAME and bind it to KEY."
  (let ((symb (intern (format "paradox-sort-by-%s" (downcase name))))
        (key (or key (substring name 0 1))))
    (eval
     `(progn
        (defun ,symb
            (invert)
          ,(format "Sort Package Menu by the %s column." name)
          (interactive "P")
          (when invert
            (setq tabulated-list-sort-key (cons ,name nil)))
          (tabulated-list--sort-by-column-name ,name))
        (define-key paradox-menu-mode-map ,(concat "S" (upcase key)) ',symb)
        (define-key paradox-menu-mode-map ,(concat "S" (downcase key)) ',symb)))))

(paradox--define-sort "Package")
(paradox--define-sort "Status")
(paradox--define-sort paradox--column-name-star "*")

(defalias 'paradox-filter-clear #'package-show-package-list
  "Clear current Package filter.
Redisplay the Packages buffer listing all packages, without
fetching the list.")

(defun paradox-filter-upgrades ()
  "Show only upgradable packages."
  (interactive)
  (if (null paradox--upgradeable-packages)
      (message "No packages have upgrades.")
    (package-show-package-list
     (mapcar #'car paradox--upgradeable-packages))
    (setq paradox--current-filter "Upgrade")))

(defvar paradox--filter-map)
(set-keymap-parent paradox-menu-mode-map package-menu-mode-map)
(define-prefix-command 'paradox--filter-map)
(define-key paradox-menu-mode-map "q" #'paradox-quit-and-close)
(define-key paradox-menu-mode-map "p" #'paradox-previous-entry)
(define-key paradox-menu-mode-map "n" #'paradox-next-entry)
(define-key paradox-menu-mode-map "k" #'paradox-previous-describe)
(define-key paradox-menu-mode-map "j" #'paradox-next-describe)
(define-key paradox-menu-mode-map "f" #'paradox--filter-map)
(define-key paradox-menu-mode-map "s" #'paradox-menu-mark-star-unstar)
(define-key paradox-menu-mode-map "h" #'paradox-menu-quick-help)
(define-key paradox-menu-mode-map "v" #'paradox-menu-visit-homepage)
(define-key paradox-menu-mode-map "l" #'paradox-menu-view-commit-list)
(define-key paradox-menu-mode-map "x" #'paradox-menu-execute)
(define-key paradox-menu-mode-map "\r" #'paradox-push-button)
(define-key paradox-menu-mode-map "F" 'package-menu-filter)
(define-key paradox--filter-map "k" #'package-menu-filter)
(define-key paradox--filter-map "f" #'package-menu-filter)
(define-key paradox--filter-map "r" #'occur)
(define-key paradox--filter-map "o" #'occur)
(define-key paradox--filter-map "u" #'paradox-filter-upgrades)
(define-key paradox--filter-map "c" #'paradox-filter-clear)


;;; Menu Mode Commands
(defun paradox-previous-entry (&optional n)
  "Move to previous entry, which might not be the previous line.
With prefix N, move to the N-th previous entry."
  (interactive "p")
  (paradox-next-entry (- n))
  (forward-line 0)
  (forward-button 1))

(defun paradox-next-entry (&optional n)
  "Move to next entry, which might not be the next line.
With prefix N, move to the N-th next entry."
  (interactive "p")
  (dotimes (it (abs n))
    (let ((d (cl-signum n)))
      (forward-line (if (> n 0) 1 0))
      (if (eobp) (forward-line -1))
      (forward-button d))))

(defun paradox-next-describe (&optional n)
  "Move to the next package and describe it.
With prefix N, move to the N-th next package instead."
  (interactive "p")
  (paradox-next-entry n)
  (call-interactively 'package-menu-describe-package))

(defun paradox-previous-describe (&optional n)
  "Move to the previous package and describe it.
With prefix N, move to the N-th previous package instead."
  (interactive "p")
  (paradox-previous-entry n)
  (call-interactively 'package-menu-describe-package))

(defun paradox-push-button ()
  "Push button under point, or describe package."
  (interactive)
  (if (get-text-property (point) 'action)
      (call-interactively 'push-button)
    (call-interactively 'package-menu-describe-package)))

(defvar paradox--key-descriptors
  '(("next," "previous," "install," "delete," ("execute," . 1) "refresh," "help")
    ("star," "visit homepage")
    ("list commits")
    ("filter by" "+" "upgrades" "regexp" "keyword")
    ("Sort by" "+" "Package name" "Status" "*(star)")))

(defun paradox-menu-quick-help ()
  "Show short key binding help for `paradox-menu-mode'.
The full list of keys can be viewed with \\[describe-mode]."
  (interactive)
  (message (mapconcat 'paradox--prettify-key-descriptor
                      paradox--key-descriptors "\n")))

(defun paradox-quit-and-close (kill)
  "Bury this buffer and close the window.
With prefix KILL, kill the buffer instead of burying."
  (interactive "P")
  (let ((log (get-buffer-window paradox--commit-list-buffer)))
    (when (window-live-p log)
      (quit-window kill log))
    (quit-window kill)))

(defun paradox-menu-visit-homepage (pkg)
  "Visit the homepage of package named PKG.
PKG is a symbol. Interactively it is the package under point."
  (interactive '(nil))
  (let ((url (paradox--package-homepage
              (paradox--get-or-return-package pkg))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage."
               (propertize (symbol-name pkg)
                           'face 'font-lock-keyword-face)))))

(defun paradox-menu-mark-star-unstar (&optional n)
  "Star or unstar a package and move to the next line.
With prefix N, mark N packages."
  (interactive "p")
  (paradox--enforce-github-token
   (unless paradox--user-starred-list
     (paradox--refresh-user-starred-list))
   ;; Get package name
   (let ((pkg (paradox--get-or-return-package nil))
         will-delete repo)
     (unless pkg (error "Couldn't find package-name for this entry"))
     ;; get repo for this package
     (setq repo (cdr-safe (assoc pkg paradox--package-repo-list)))
     ;; (Un)Star repo
     (if (not repo)
         (message "This package is not a GitHub repo.")
       (setq will-delete (member repo paradox--user-starred-list))
       (paradox--star-repo repo will-delete)
       (cl-incf (cdr (assoc pkg paradox--star-count))
                (if will-delete -1 1))
       (tabulated-list-set-col paradox--column-name-star
                               (paradox--package-star-count pkg)))))
  (forward-line 1))

(defun paradox-menu-view-commit-list (pkg)
  "Visit the commit list of package named PKG.
PKG is a symbol. Interactively it is the package under point."
  (interactive '(nil))
  (let* ((name (paradox--get-or-return-package pkg))
         (repo (cdr (assoc name paradox--package-repo-list))))
    (if repo
        (with-selected-window
            (display-buffer (get-buffer-create paradox--commit-list-buffer))
          (paradox-commit-list-mode)
          (setq paradox--package-repo repo)
          (setq paradox--package-name name)
          (setq paradox--package-version
                (paradox--get-installed-version name))
          (setq paradox--package-tag-commit-alist
                (paradox--get-tag-commit-alist repo))
          (paradox--commit-list-update-entries)
          (tabulated-list-print))
      (message "Package %s is not a GitHub repo." pkg))))


;;; Execution Hook
(defvar paradox-after-execute-functions
  '(paradox--activate-if-asynchronous
    paradox--refresh-package-buffer
    paradox--report
    )
  "List of functions run after performing package transactions.
These are run after a set of installation, deletion, or upgrades
has been performed. Each function in this hook must take a single
argument. An associative list of the form

    ((SYMBOL . DATA) (SYMBOL . DATA) ...)

This list contains the following entries, describing what
occurred during the execution:

  SYMBOL      DATA
  `installed' List of installed packages.
  `deleted'   List of deleted packages.
  `activated' List of activated packages.
  `error'     List of errors.
  `async'     Non-nil if transaction was performed asynchronously.
  `noquery'   The NOQUERY argument given to `paradox-menu-execute'.")
(put 'risky-local-variable-p 'paradox-after-execute-functions t)

(defun paradox--refresh-package-buffer (_)
  "Refresh the *Packages* buffer, if it exists."
  (let ((buf (get-buffer "*Packages*")))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (paradox--generate-menu t t)))))

(defun paradox--activate-if-asynchronous (alist)
  "Activate packages after an asynchronous operation."
  (let-alist alist
    (when .async
      (mapc #'package-activate-1 .activated))))

(defcustom paradox-async-display-buffer-function #'display-buffer
  "Function used to display the *Paradox Report* buffer after an asynchronous upgrade.
Set this to nil to avoid displaying the buffer. Or set this to a
function like `display-buffer' or `pop-to-buffer'.

This is only used if `paradox-menu-execute' was given a non-nil
NOQUERY argument. Otherwise, only a message is displayed."
  :type '(choice (const :tag "Don't display the buffer" nil)
                 function)
  :package-version '(paradox . "2.0"))

(defun paradox--report (alist)
  "Print a transaction report in *Package Report* buffer.
Possibly display the buffer or message the user depending on the
situation."
  (let-alist alist
    (let ((buf (get-buffer-create "*Paradox Report*"))
          (inhibit-read-only t))
      (with-current-buffer buf
        (goto-char (point-max))
        ;; TODO: Write our own mode for this.
        (special-mode)
        (insert "\n\n")
        (save-excursion
          (insert (format-time-string "Package transaction finished. %c\n"))
          (when .error
            (insert "Errors:\n  " (mapconcat #'cdr .error "\n ") "\n\n"))
          (when .installed
            (insert "Installed:\n  "
                    (mapconcat (lambda (x) (symbol-name (package-desc-name x))) .installed "\n  ")
                    "\n\n"))
          (when .deleted
            (insert "Deleted:\n  "
                    (mapconcat (lambda (x) (symbol-name (package-desc-name x))) .deleted "\n  ")
                    "\n\n"))))
      (cond
       ;; The user has never seen the packages in this transaction. So
       ;; we display them in a buffer.
       ((and .noquery (not .async)) (pop-to-buffer buf))
       ;; If we're async, the user might be doing something else, so
       ;; we don't steal focus.
       ((and .noquery .async paradox-async-display-buffer-function)
        (funcall paradox-async-display-buffer-function buf))
       ;; The user was asked for confirmation, so they know what's
       ;; going on. We just report conclusion.
       (t (message
           "%s See the buffer *Paradox Report* for more details."
           (paradox--format-message nil .installed .deleted)))))))


;;; Execution
(defun paradox-menu-execute (&optional noquery)
  "Perform marked Package Menu actions.
Packages marked for installation are downloaded and installed;
packages marked for deletion are removed.

Afterwards, if `paradox-automatically-star' is t, automatically
star new packages, and unstar removed packages. Upgraded packages
aren't changed.

Synchronicity of the actions depends on
`paradox-execute-asynchronously'. Optional argument NOQUERY
non-nil means do not ask the user to confirm. If asynchronous,
never ask anyway."
  (interactive "P")
  (unless (derived-mode-p 'paradox-menu-mode)
    (error "The current buffer is not in Paradox Menu mode"))
  (when (and (stringp paradox-github-token)
             (eq paradox-automatically-star 'unconfigured))
    (customize-save-variable
     'paradox-automatically-star
     (y-or-n-p "When you install new packages would you like them to be automatically starred?\n(They will be unstarred when you delete them) ")))
  (paradox--menu-execute-1 noquery))

(defmacro paradox--perform-package-transaction (install delete)
  "Install all packages from INSTALL and delete those from DELETE.
Return an alist with properties listing installed,
deleted, and activated packages, and errors."
  `(let (activated installed deleted errored)
     (advice-add #'package-activate-1 :after
                 (lambda (pkg &rest _)
                   (ignore-errors (add-to-list 'activated pkg 'append)))
                 '((name . paradox--track-activated)))
     (dolist (pkg ,install)
       (condition-case err
           (progn (package-install pkg) (push pkg installed))
         (error (push err errored))))
     (dolist (pkg ,delete)
       (condition-case err
           (progn (package-delete pkg) (push pkg deleted))
         (error (push err errored))))
     (advice-remove #'package-activate-1 'paradox--track-activated)
     (list (cons 'installed (nreverse installed))
           (cons 'deleted (nreverse deleted))
           ;; Because we used 'append, this is in the right order.
           (cons 'activated activated)
           (cons 'error (nreverse errored)))))

(defun paradox--menu-execute-1 (&optional noquery)
  (let ((before-alist (paradox--repo-alist))
        install-list delete-list)
    (save-excursion
      (goto-char (point-min))
      (let ((p (point))
            (inhibit-read-only t))
        (while (not (eobp))
          (let ((c (char-after)))
            (if (eq c ?\s)
                (forward-line 1)
              (push (tabulated-list-get-id)
                    (cl-case c
                      (?D delete-list)
                      (?I install-list)))
              (delete-region p (point))
              (forward-line 1)
              (setq p (point)))))
        (when (or delete-list install-list)
          (delete-region p (point))
          (ignore-errors
            (set-window-start (selected-window) (point-min))))))
    (if (not (or delete-list install-list))
        (message "No operations specified.")
      ;; Display the transaction about to be performed.
      (setq paradox--current-filter "Executing Transaction")
      ;; Confirm with the user.
      (when (or noquery
                (y-or-n-p (paradox--format-message 'question install-list delete-list)))
        ;; Background or foreground?
        (if (not (cl-case paradox-execute-asynchronously
                   ((nil) nil)
                   ((ask)
                    (if noquery nil
                      (y-or-n-p "Execute in the background? (see `paradox-execute-asynchronously')")))
                   (t t)))
            ;; Synchronous execution
            (progn
              (let ((alist (paradox--perform-package-transaction install-list delete-list)))
                (run-hook-with-args 'paradox-after-execute-functions
                                    `((noquery . ,noquery) (async . nil) ,@alist)))
              (when (and (stringp paradox-github-token) paradox-automatically-star)
                (paradox--post-execute-star-unstar before-alist (paradox--repo-alist))))
          ;; Async execution
          (unless (require 'async nil t)
            (error "For asynchronous execution please install the `async' package"))
          ;; We have to do this with eval, because `async-start' is a
          ;; macro and it might not have been defined at compile-time.
          (eval
           `(async-start
             (lambda ()
               (setq package-user-dir ,package-user-dir
                     package-archives ',package-archives
                     package-archive-contents ',package-archive-contents)
               (package-initialize)
               (let ((alist ,(macroexpand
                              `(paradox--perform-package-transaction ',install-list ',delete-list))))
                 (list package-alist
                       package-archive-contents
                       ;; This is the alist that will be passed to the hook.
                       (cons '(noquery . ,noquery) (cons '(async . t) alist)))))
             (lambda (x)
               (setq package-alist (pop x)
                     package-archive-contents (pop x))
               (run-hook-with-args 'paradox-after-execute-functions (pop x))
               (paradox--post-execute-star-unstar ',before-alist (paradox--repo-alist))))))))))

(defun paradox--format-message (question-p install-list delete-list)
  "Format a message regarding a transaction.
If QUESTION-P is non-nil, format a question suitable for
`y-or-n-p', otherwise format a report in the past sense.
INSTALL-LIST and DELETE-LIST are a list of packages about to be
installed and deleted, respectively."
  (concat
   (when install-list
     (let ((len (length install-list)))
       (format "Install%s %d package%s"
         (if question-p "" "ed")
         len
         (if (> len 1) "s" ""))))
   (when (and install-list (not delete-list))
     (if question-p "? " "."))
   (when (and install-list delete-list)
     ", and ")
   (when delete-list
     (let ((len (length delete-list)))
       (format "Delete%s %d package%s%s"
         (if question-p "" "d")
         len
         (if (> len 1) "s" "")
         (if question-p "? " "."))))))

(defun paradox--post-execute-star-unstar (before after)
  "Star repos in AFTER absent from BEFORE, unstar vice-versa."
  (mapc #'paradox--star-repo
    (-difference (-difference after before) paradox--user-starred-list))
  (mapc #'paradox--unstar-repo
    (-intersection (-difference before after) paradox--user-starred-list)))


;;; External Commands
;;;###autoload
(defun paradox-list-packages (no-fetch)
  "Improved version of `package-list-packages'. The heart of Paradox.
Function is equivalent to `package-list-packages' (including the
prefix NO-FETCH), but the resulting Package Menu is improved in
several ways.

Among them:

1. Uses `paradox-menu-mode', which has more functionality and
keybinds than `package-menu-mode'.

2. Uses some font-locking to improve readability.

3. Optionally shows the number GitHub stars and Melpa downloads
for packages.

4. Adds useful information in the mode-line."
  (interactive "P")
  (when (paradox--check-github-token)
    (paradox-enable)
    (unless no-fetch (paradox--refresh-star-count))
    (package-list-packages no-fetch)))

;;;###autoload
(defun paradox-upgrade-packages (&optional no-fetch)
  "Upgrade all packages. No questions asked.
This function is equivalent to `list-packages', followed by a
`package-menu-mark-upgrades' and a `package-menu-execute'. Except
the user isn't asked to confirm deletion of packages.

If `paradox-execute-asynchronously' is non-nil, part of this
operation may be performed in the background.

The NO-FETCH prefix argument is passed to `list-packages'. It
prevents re-download of information about new versions. It does
not prevent downloading the actual packages (obviously)."
  (interactive "P")
  (save-window-excursion
    (paradox-list-packages no-fetch)
    (paradox-filter-upgrades)
    (package-menu-mark-upgrades)
    (paradox-menu-execute 'noquery)))

(defun paradox-enable ()
  "Enable paradox, overriding the default package-menu."
  (interactive)
  (paradox--override-definition 'package-menu--print-info 'paradox--print-info)
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
  "Non-nil if paradox has been activated."
  paradox--backups)


;;; `paradox-menu-mode' configuration
(defcustom paradox-status-face-alist
  '(("built-in"  . font-lock-builtin-face)
    ("available" . default)
    ("new"       . bold)
    ("held"      . font-lock-constant-face)
    ("disabled"  . font-lock-warning-face)
    ("installed" . font-lock-comment-face)
    ("deleted"   . font-lock-comment-face)
    ("unsigned"  . font-lock-warning-face))
  "List of (\"STATUS\" . FACE) cons cells.
When displaying the package menu, FACE will be used to paint the
Version, Status, and Description columns of each package whose
status is STATUS."
  :type '(repeat (cons string face))
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defcustom paradox-homepage-button-string "h"
  "String used to for the link that takes you to a package's homepage."
  :type 'string
  :group 'paradox
  :package-version '(paradox . "0.10"))

(defcustom paradox-use-homepage-buttons t
  "If non-nil a button will be added after the name of each package.
This button takes you to the package's homepage."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "0.10"))

(defcustom paradox-lines-per-entry 1
  "Number of lines used to display each entry in the Package Menu.
1 Gives you the regular package menu.
2 Displays the description on a separate line below the entry.
3+ Adds empty lines separating the entries."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.10"))


;;; Building the packages buffer.
(defun paradox--truncate-string-to-width (&rest args)
  "Like `truncate-string-to-width', but uses \"…\" on package buffer.
All arguments STR, END-COLUMN, START-COLUMN, PADDING, and
ELLIPSIS are passed to `truncate-string-to-width'.

\(fn STR END-COLUMN &optional START-COLUMN PADDING ELLIPSIS)"
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

(defvar desc-suffix nil)
(defvar desc-prefix nil)

(defun paradox--print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [STAR NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (car pkg))
         (status  (cdr pkg))
         (face (or (cdr (assoc-string status paradox-status-face-alist))
                   'font-lock-warning-face))
         (url (paradox--package-homepage pkg-desc))
         (name (symbol-name (package-desc-name pkg-desc)))
         (name-length (length name))
         (button-length (length paradox-homepage-button-string)))
    (paradox--incf status)
    (list pkg-desc
          `[,(concat
              (propertize name
                          'face 'paradox-name-face
                          'button t
                          'follow-link t
                          'help-echo (format "Package: %s" name)
                          'package-desc pkg-desc
                          'action 'package-menu-describe-package)
              (if (and paradox-use-homepage-buttons url
                       (< (+ name-length button-length) paradox-column-width-package))
                  (concat
                   (make-string (- paradox-column-width-package name-length button-length) ?\s)
                   (propertize paradox-homepage-button-string
                               'face 'paradox-homepage-button-face
                               'mouse-face 'custom-button-mouse
                               'help-echo (format "Visit %s" url)
                               'button t
                               'follow-link t
                               'action 'paradox-menu-visit-homepage))
                ""))
            ,(propertize (package-version-join
                          (package-desc-version pkg-desc))
                         'font-lock-face face)
            ,(propertize status 'font-lock-face face)
            ,@(if (cdr package-archives)
                  (list (propertize (or (package-desc-archive pkg-desc) "")
                                    'font-lock-face 'paradox-archive-face)))
            ,@(paradox--count-print (package-desc-name pkg-desc))
            ,(propertize ;; (package-desc-summary pkg-desc)
              (concat desc-prefix (package-desc-summary pkg-desc) desc-suffix) ;└╰
              'font-lock-face
              (if (> paradox-lines-per-entry 1)
                  'paradox-description-face-multiline
                'paradox-description-face))])))

(defun paradox--count-print (pkg)
  "Return counts of PKG as a package-desc list."
  (append
   (when (and paradox-display-star-count (listp paradox--star-count))
     (list (paradox--package-star-count pkg)))
   (when (and paradox-display-download-count (listp paradox--download-count))
     (list (paradox--package-download-count pkg)))))

(defun paradox--package-download-count (pkg)
  "Return propertized string with the download count of PKG."
  (let ((c (cdr-safe (assoc pkg paradox--download-count))))
    (propertize
     (if (numberp c)
         (if (> c 999) (format "%sK" (truncate c 1000)) (format "%s" c))
       " ")
     'face 'paradox-download-face
     'value (or c 0))))

(defun paradox--package-homepage (pkg)
  "PKG can be the package-name symbol or a package-desc object."
  (let* ((object   (if (symbolp pkg) (cadr (assoc pkg package-archive-contents)) pkg))
         (name     (if (symbolp pkg) pkg (package-desc-name pkg)))
         (extras   (package-desc-extras object))
         (homepage (and (listp extras) (cdr-safe (assoc :url extras)))))
    (or homepage
        (and (setq extras (cdr (assoc name paradox--package-repo-list)))
             (format "https://github.com/%s" extras)))))
(defun paradox--get-or-return-package (pkg)
  (if (or (markerp pkg) (null pkg))
      (if (derived-mode-p 'package-menu-mode)
          (package-desc-name (tabulated-list-get-id))
        (error "Not in Package Menu"))
    pkg))

(defun paradox--incf (status)
  "Increment the count for STATUS on `paradox--package-count'.
Also increments the count for \"total\"."
  (paradox--inc-count status)
  (unless (string= status "obsolete")
    (paradox--inc-count "total")))

(defun paradox--inc-count (string)
  "Increment the cdr of (assoc-string STRING paradox--package-count)."
  (let ((cons (assoc-string string paradox--package-count)))
    (setcdr cons (1+ (cdr cons)))))

(defun paradox--entry-star-count (entry)
  "Get the star count of the package in ENTRY."
  (paradox--package-star-count
   ;; The package symbol should be in the ID field, but that's not mandatory,
   (or (ignore-errors (elt (car entry) 1))
       ;; So we also try interning the package name.
       (intern (car (elt (cadr entry) 0))))))

(defvar paradox--user-starred-list nil)

(defun paradox--refresh-star-count ()
    "Download the star-count file and populate the respective variable."
    (interactive)
    (unwind-protect
        (with-current-buffer
        (url-retrieve-synchronously paradox--star-count-url)
        (when (search-forward "\n\n" nil t)
            (setq paradox--star-count (read (current-buffer)))
            (setq paradox--package-repo-list (read (current-buffer)))
            (setq paradox--download-count (read (current-buffer))))
        (kill-buffer))
        (unless (and (listp paradox--star-count)
               (listp paradox--package-repo-list)
               (listp paradox--download-count))
        (message "[Paradox] Error downloading the list of repositories. This might be a proxy"))
        (unless (listp paradox--download-count)
        (setq paradox--download-count nil))
        (unless (listp paradox--package-repo-list)
        (setq paradox--package-repo-list nil))
        (unless (listp paradox--star-count)
        (setq paradox--star-count nil)))
    (when (stringp paradox-github-token)
        (paradox--refresh-user-starred-list)))

(defun paradox--package-star-count (package)
  "Get the star count of PACKAGE."
  (let ((count (cdr (assoc package paradox--star-count)))
        (repo (cdr-safe (assoc package paradox--package-repo-list))))
    (propertize
     (format "%s" (or count ""))
     'face
     (if (and repo (assoc-string repo paradox--user-starred-list))
         'paradox-starred-face
       'paradox-star-face))))

(defvar paradox--column-index-star nil)
(defvar paradox--column-index-download nil)

(defun paradox--star-predicate (A B)
  "Non-nil t if star count of A is larget than B."
  (> (string-to-number (elt (cadr A) paradox--column-index-star))
     (string-to-number (elt (cadr B) paradox--column-index-star))))
(defun paradox--download-predicate (A B)
  "Non-nil t if download count of A is larget than B."
  (> (get-text-property 0 'value (elt (cadr A) paradox--column-index-download))
     (get-text-property 0 'value (elt (cadr B) paradox--column-index-download))))

(defun paradox--generate-menu (remember-pos packages &optional keywords)
  "Populate the Package Menu, without hacking into the header-format.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (mapc (lambda (x) (setf (cdr x) 0)) paradox--package-count)
  (let ((desc-prefix (if (> paradox-lines-per-entry 1) " \n      " ""))
        (desc-suffix (make-string (max 0 (- paradox-lines-per-entry 2)) ?\n)))
    (paradox-menu--refresh packages keywords))
  (setq paradox--current-filter
        (if keywords (mapconcat 'identity keywords ",")
          nil))
  (let ((idx (paradox--column-index "Package")))
    (setcar (elt tabulated-list-format idx)
            (if keywords
                (concat "Package[" paradox--current-filter "]")
              "Package")))
  (tabulated-list-print remember-pos)
  (tabulated-list-init-header)
  (paradox--update-mode-line)
  (paradox-refresh-upgradeable-packages))

(defalias 'paradox-menu--refresh 'package-menu--refresh)

(defun paradox--column-index (regexp)
  "Find the index of the column that matches REGEXP."
  (cl-position (format "\\`%s\\'" (regexp-quote regexp)) tabulated-list-format
               :test (lambda (x y) (string-match x (or (car-safe y) "")))))

(defun paradox--count-format ()
  "List of star/download counts to be used as part of the entry."
  (remove
   nil
   (list
    (when paradox-display-star-count
      (list paradox--column-name-star paradox-column-width-star
            'paradox--star-predicate :right-align t))
    (when paradox-display-download-count
      (list paradox--column-name-download paradox-column-width-download
            'paradox--download-predicate :right-align t)))))

(defun paradox--archive-format ()
  "List containing archive to be used as part of the entry."
  (when (cdr package-archives)
    (list (list "Archive"
                (apply 'max (mapcar 'length (mapcar 'car package-archives)))
                'package-menu--archive-predicate))))

(add-hook 'paradox-menu-mode-hook 'paradox-refresh-upgradeable-packages)


;;; Mode-line Construction
(defcustom paradox-local-variables
  '(mode-line-mule-info
    mode-line-client
    mode-line-remote mode-line-position
    column-number-mode size-indication-mode)
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

(defun paradox--build-buffer-id (st n)
  "Return a list that propertizes ST and N for the mode-line."
  `((:propertize ,st
                 face paradox-mode-line-face)
    (:propertize ,(int-to-string n)
                 face mode-line-buffer-id)))

(defun paradox--update-mode-line ()
  "Update `mode-line-format'."
  (mapc #'paradox--set-local-value paradox-local-variables)
  (let ((total-lines (int-to-string (line-number-at-pos (point-max)))))
    (paradox--update-mode-line-front-space total-lines)
    (paradox--update-mode-line-buffer-identification total-lines))
  (set-face-foreground
   'paradox-mode-line-face
   (-when-let (fg (or (face-foreground 'mode-line-buffer-id nil t)
                      (face-foreground 'default nil t)))
     (if (> (color-distance "white" fg)
            (color-distance "black" fg))
         "black" "white"))))

(defun paradox--update-mode-line-buffer-identification (total-lines)
  "Update `mode-line-buffer-identification'.
TOTAL-LINES is currently unused."
  (setq mode-line-buffer-identification
        (list
         (list 'paradox-display-buffer-name
               (propertized-buffer-identification
                (format "%%%sb" (length (buffer-name)))))
         '(paradox--current-filter (:propertize ("[" paradox--current-filter "]") face paradox-mode-line-face))
         '(paradox--upgradeable-packages-any?
           (:eval (paradox--build-buffer-id " Upgrade:" paradox--upgradeable-packages-number)))
         '(package-menu--new-package-list
           (:eval (paradox--build-buffer-id " New:" (paradox--cas "new"))))
         (paradox--build-buffer-id " Installed:" (+ (paradox--cas "installed") (paradox--cas "unsigned")))
         `(paradox--current-filter
           "" ,(paradox--build-buffer-id " Total:" (length package-archive-contents))))))

(defun paradox--update-mode-line-front-space (total-lines)
  "Update `mode-line-front-space'.
TOTAL-LINES is the number of lines in the buffer."
  (if (memq 'sml/post-id-separator mode-line-format)
      (progn
        (add-to-list (make-local-variable 'mode-line-front-space)
                     (propertize " (" 'face 'sml/col-number))
        (setq column-number-mode line-number-mode)
        (set (make-local-variable 'sml/numbers-separator) "")
        (set (make-local-variable 'sml/col-number-format)
             (format "/%s)" total-lines))
        (set (make-local-variable 'sml/line-number-format)
             (format "%%%sl" (length total-lines)))
        (make-local-variable 'sml/position-construct)
        (sml/compile-position-construct))
    (set (make-local-variable 'mode-line-front-space)
         `(line-number-mode
           ("(" (:propertize ,(format "%%%sl" (length total-lines)) face mode-line-buffer-id) "/"
            ,total-lines ")")))
    (set (make-local-variable 'mode-line-modified) nil)))

(defun paradox--set-local-value (x)
  "Locally set value of (car X) to (cdr X)."
  (let ((sym (or (car-safe x) x)))
    (when (boundp sym)
      (set (make-local-variable sym) (cdr-safe x)))))

(defun paradox--repo-alist ()
  "List of known repos."
  (cl-remove-duplicates
   (remove nil
           (--map (cdr-safe (assoc (car it) paradox--package-repo-list))
                  package-alist))))

(defun paradox--prettify-key-descriptor (desc)
  "Prettify DESC to be displayed as a help menu."
  (if (listp desc)
      (if (listp (cdr desc))
          (mapconcat 'paradox--prettify-key-descriptor desc "   ")
        (let ((place (cdr desc))
              (out (car desc)))
          (setq out (propertize out 'face 'paradox-comment-face))
          (add-text-properties place (1+ place) '(face paradox-highlight-face) out)
          out))
    (paradox--prettify-key-descriptor (cons desc 0))))

(defun paradox--full-name-reader ()
  "Return all \"full_name\" properties in the buffer. Much faster than `json-read'."
  (let (out)
    (while (search-forward-regexp
            "^ *\"full_name\" *: *\"\\(.*\\)\", *$" nil t)
      (add-to-list 'out (match-string-no-properties 1)))
    (goto-char (point-max))
    out))

;;;###autoload
(defun paradox-require (feature &optional filename noerror package refresh)
  "A replacement for `require' which also installs the feature if it is absent.
- If FEATURE is present, `require' it and return t.

- If FEATURE is not present, install PACKAGE with `package-install'.
If PACKAGE is nil, assume FEATURE is the package name.
After installation, `require' FEATURE.

FILENAME is passed to `require'.

If NOERROR is non-nil, don't complain if the feature couldn't be
installed, just return nil.

By default, the current package database (stored in
`package-archive-contents') is only updated if it is empty.
Passing a non-nil REFRESH argument forces this update."
  (or (require feature filename t)
      (let ((package (or package
                         (if (stringp feature)
                             (intern feature)
                           feature))))
        (require 'package)
        (unless (and package-archive-contents (null refresh))
          (package-refresh-contents))
        (and (condition-case e
                 (package-install package)
               (error (if noerror nil (error (cadr e)))))
             (require feature filename noerror)))))

(provide 'paradox)
;;; paradox.el ends here.
