;;; paradox.el --- A modern Packages Menu. Colored, with package ratings, and customizable.

;; Copyright (C) 2014 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/paradox
;; Version: 1.2.1
;; Keywords: package packages mode-line
;; Package-Requires: ((emacs "24.1") (tabulated-list "1.0") (package "1.0") (dash "2.6.0") (cl-lib "1.0") (json "1.3"))
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
;; 1.1   - 2014/07/02 - NEW FUNCTION: paradox-require.
;; 1.2   - 2014/05/15 - Integration with smart-mode-line.
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
(defconst version "1.2.1" "Version of the paradox.el package.")
(defun bug-report ()
  "Opens github issues page in a web browser. Please send any bugs you find.
Please include your Emacs and paradox versions."
  (interactive)
  (message "Your paradox-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           version emacs-version)
  (browse-url "https://github.com/Bruce-Connor/paradox/issues/new"))
(defun customize ()
  "Open the customization menu in the `paradox' group."
  (interactive)
  (customize-group 'paradox t))
(defgroup paradox nil
  "Customization group for paradox."
  :prefix "paradox-"
  :group 'emacs
  :package-version '(paradox . "0.1"))
(defun -compat-p ()
  "Non-nil if we need to enable pre-24.4 compatibility features."
  (version< emacs-version "24.3.50"))

(defcustom column-width-package  18
  "Width of the \"Package\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom column-width-version  9
  "Width of the \"Version\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom column-width-status  10
  "Width of the \"Status\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defcustom column-width-star 4
  "Width of the \"Star\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.1"))

(defvar -column-name-star
  (if (char-displayable-p ?★) "★" "*"))

(defcustom column-width-download 4
  "Width of the \"Download Count\" column."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "1.1"))

(defvar -column-name-download
  (if (char-displayable-p ?↓) "↓" "DC"))

(defcustom github-token nil
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

DISCLAIMER:
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account."
  :type 'string
  :group 'paradox
  :package-version '(paradox . "0.2"))

(defcustom automatically-star 'unconfigured
  "When you install new packages, should they be automatically starred?
NOTE: This variable has no effect if `paradox-github-token' isn't set.

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

(defcustom display-star-count t
  "If non-nil, adds a \"Star\" column to the Package Menu."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "1.1"))

(defcustom display-download-count t
  "If non-nil, adds a \"Download\" column to the Package Menu."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "1.1"))

(defface mode-line-face
  '((t :inherit mode-line-buffer-id :weight normal :foreground "Black"))
  "Face used on the package's name."
  :group 'paradox)
(defface name-face
  '((t :inherit link))
  "Face used on the package's name."
  :group 'paradox)
(defface homepage-button-face
  '((t :underline t :inherit font-lock-comment-face))
  "Face used on the homepage button."
  :group 'paradox)
;; (defface version-face
;;   '((t :inherit default))
;;   "Face used on the version column."
;;   :group 'paradox)
(defface archive-face
  '((t :inherit paradox-comment-face))
  "Face used on the archive column."
  :group 'paradox)
(defface star-face
  '((t :inherit font-lock-string-face))
  "Face used on the star column, for packages you haven't starred."
  :group 'paradox)
(defface starred-face
  '((t :weight bold :inherit paradox-star-face))
  "Face used on the star column, for packages you have starred."
  :group 'paradox)
(defface download-face
  '((t :inherit font-lock-keyword-face))
  "Face used on the Downloads column."
  :group 'paradox)
(defface description-face
  '((t :inherit default))
  "Face used on the description column.
If `paradox-lines-per-entry' > 1, the face
`paradox-description-face-multiline' is used instead."
  :group 'paradox)
(defface description-face-multiline
  '((t :inherit font-lock-doc-face))
  "Face used on the description column when `paradox-lines-per-entry' > 1.
If `paradox-lines-per-entry' = 1, the face
`paradox-description-face' is used instead."
  :group 'paradox)

(defface comment-face
  '((((background light)) :foreground "Grey30")
    (((background dark)) :foreground "Grey60"))
  "Face used on faded out stuff."
  :group 'paradox)
(defface highlight-face
  '((t :weight bold :inherit font-lock-variable-name-face))
  "Face used on highlighted stuff."
  :group 'paradox)

(defvar -star-count nil)
(defvar -download-count nil)
(defvar -package-repo-list nil)

(defvar -star-count-url
  "https://raw.githubusercontent.com/Bruce-Connor/paradox/data/data"
  "Address of the raw star-count file.")

(defvar -package-count
  '(("total" . 0) ("built-in" . 0)
    ("obsolete" . 0) ("deleted" . 0)
    ("available" . 0) ("new" . 0)
    ("held" . 0) ("disabled" . 0)
    ("installed" . 0) ("unsigned" . 0)))

(defvar-local -current-filter nil)

(defvar -commit-list-buffer "*Package Commit List*")

(defvar -truncate-string-to-width-backup)

(defmacro -cas (string)
  "Same as (cdr (assoc-string ,STRING paradox--package-count))."
  `(cdr (assoc-string ,string paradox--package-count)))

(defvar -data-url "https://raw.github.com/Bruce-Connor/paradox/data/full"
  "Address of the raw data file.")

(defvar menu-mode-map package-menu-mode-map)
(defvar -filter-map)
(define-prefix-command 'paradox--filter-map)
(define-key menu-mode-map "q" #'quit-and-close)
(define-key menu-mode-map "p" #'previous-entry)
(define-key menu-mode-map "n" #'next-entry)
(define-key menu-mode-map "k" #'previous-describe)
(define-key menu-mode-map "j" #'next-describe)
(define-key menu-mode-map "f" #'paradox--filter-map)
(define-key menu-mode-map "s" #'menu-mark-star-unstar)
(define-key menu-mode-map "h" #'menu-quick-help)
(define-key menu-mode-map "v" #'menu-visit-homepage)
(define-key menu-mode-map "l" #'menu-view-commit-list)
(define-key menu-mode-map "\r" #'push-button)
(define-key menu-mode-map "F" 'package-menu-filter)
(define-key -filter-map "k" #'package-menu-filter)
(define-key -filter-map "f" #'package-menu-filter)
(define-key -filter-map "r" #'occur)
(define-key -filter-map "o" #'occur)
(define-key -filter-map "u" #'filter-upgrades)

(defun -define-sort (name &optional key)
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

(-define-sort "Package")
(-define-sort "Status")
(-define-sort -column-name-star "*")

(defun next-describe (n)
  "Move to the next package and describe it.
With prefix N, move to the N-th next package instead."
  (interactive "p")
  (next-entry n)
  (call-interactively 'package-menu-describe-package))

(defun previous-describe (n)
  "Move to the previous package and describe it.
With prefix N, move to the N-th previous package instead."
  (interactive "p")
  (previous-entry n)
  (call-interactively 'package-menu-describe-package))

(defun push-button ()
  "Push button under point, or describe package."
  (interactive)
  (if (get-text-property (point) 'action)
      (call-interactively 'push-button)
    (call-interactively 'package-menu-describe-package)))

(defvar -key-descriptors
  '(("next," "previous," "install," "delete," ("execute," . 1) "refresh," "help")
    ("star," "visit homepage")
    ("list commits")
    ("filter by" "+" "upgrades" "regexp" "keyword")
    ("Sort by" "+" "Package name" "Status" "*(star)")))

(defun menu-quick-help ()
  "Show short key binding help for `paradox-menu-mode'.
The full list of keys can be viewed with \\[describe-mode]."
  (interactive)
  (message (mapconcat #'-prettify-key-descriptor
                      -key-descriptors "\n")))

(defun quit-and-close (kill)
  "Bury this buffer and close the window.
With prefix KILL, kill the buffer instead of burying."
  (interactive "P")
  (if -current-filter
      (package-show-package-list)
    (let ((log (get-buffer-window -commit-list-buffer)))
      (when (window-live-p log)
        (quit-window kill log))
      (quit-window kill))))

;;;###autoload
(defun -refresh-star-count ()
  "Download the star-count file and populate the respective variable."
  (interactive)
  (with-current-buffer
      (url-retrieve-synchronously -star-count-url)
    (when (search-forward "\n\n" nil t)
      (setq -star-count (read (current-buffer)))
      (setq -package-repo-list (read (current-buffer)))
      (setq -download-count (ignore-errors (read (current-buffer)))))
    (kill-buffer))
  (when (stringp github-token)
    (-refresh-user-starred-list)))

(defcustom hide-buffer-identification t
  "If non-nil, the name of the buffer will not be displayed in the mode-line."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "0.5"))
(defvaralias 'hide-buffer-name 'hide-buffer-identification)

(defun -build-buffer-id (st n)
  "Return a list that propertizes ST and N for the mode-line."
  `((:propertize ,st
                 face paradox-mode-line-face)
    (:propertize ,(int-to-string n)
                 face mode-line-buffer-id)))

;;;###autoload
(defun list-packages (no-fetch)
  "Improved version of `package-list-packages'. The heart of paradox.
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
  (when (-check-github-token)
    (enable)
    (unless no-fetch (-refresh-star-count))
    (package-list-packages no-fetch)))

(defun enable ()
  "Enable paradox, overriding the default package-menu."
  (interactive)
  (ad-enable-advice 'package-menu-execute 'around
                    'paradox-around-package-menu-execute-advice)
  (ad-activate 'package-menu-execute)
  (if (-compat-p)
      (progn
        (require 'paradox-compat)
        (-override-definition #'package-menu--print-info #'-print-info-compat))
    (-override-definition #'package-menu--print-info #'-print-info))
  (-override-definition #'package-menu--generate #'-generate-menu)
  (-override-definition #'truncate-string-to-width #'-truncate-string-to-width)
  (-override-definition #'package-menu-mode #'menu-mode))

(defvar -backups nil)

(defun disable ()
  "Disable paradox, and go back to regular package-menu."
  (interactive)
  (ad-disable-advice 'package-menu-execute 'around
                    'paradox-around-package-menu-execute-advice)
  (dolist (it -backups)
    (message "Restoring %s to %s" (car it) (eval (cdr it)))
    (fset (car it) (eval (cdr it))))
  (setq -backups nil))

(defun -override-definition (sym newdef)
  "Temporarily override SYM's function definition with NEWDEF.
The original definition is saved to paradox--SYM-backup."
  (let ((backup-name (intern (format "paradox--%s-backup" sym)))
        (def (symbol-function sym)))
    (unless (assoc sym -backups)
      (message "Overriding %s with %s" sym newdef)
      (eval (list 'defvar backup-name nil))
      (add-to-list 'paradox--backups (cons sym backup-name))
      (set backup-name def)
      (fset sym newdef))))

;;; Right now this is trivial, but we leave it as function so it's easy to improve.
(defun -active-p ()
  "Non-nil if paradox has been activated."
  (null (null -backups)))

(defun -truncate-string-to-width (&rest args)
  "Like `truncate-string-to-width', but uses \"…\" on package buffer.
All arguments STR, END-COLUMN, START-COLUMN, PADDING, and
ELLIPSIS are passed to `truncate-string-to-width'.

\(fn STR END-COLUMN &optional START-COLUMN PADDING ELLIPSIS)"
  (when (and (eq major-mode 'paradox-menu-mode)
             (eq t (nth 4 args)))
    (setf (nth 4 args) (if (char-displayable-p ?…) "…" "$")))
  (apply -truncate-string-to-width-backup args))

(defvar -upgradeable-packages nil)
(defvar -upgradeable-packages-number nil)
(defvar -upgradeable-packages-any? nil)

(defun refresh-upgradeable-packages ()
  "Refresh the list of upgradeable packages."
  (interactive)
  (setq -upgradeable-packages (package-menu--find-upgrades))
  (setq -upgradeable-packages-number
        (length -upgradeable-packages))
  (setq -upgradeable-packages-any?
        (> -upgradeable-packages-number 0)))

(defcustom status-face-alist
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

(defcustom homepage-button-string "h"
  "String used to for the link that takes you to a package's homepage."
  :type 'string
  :group 'paradox
  :package-version '(paradox . "0.10"))

(defcustom use-homepage-buttons t
  "If non-nil a button will be added after the name of each package.
This button takes you to the package's homepage."
  :type 'boolean
  :group 'paradox
  :package-version '(paradox . "0.10"))

(defvar desc-suffix nil)
(defvar desc-prefix nil)

(defcustom lines-per-entry 1
  "Number of lines used to display each entry in the Package Menu.
1 Gives you the regular package menu.
2 Displays the description on a separate line below the entry.
3+ Adds empty lines separating the entries."
  :type 'integer
  :group 'paradox
  :package-version '(paradox . "0.10"))

(defvar-local -repo nil)

(defun -print-info (pkg)
  "Return a package entry suitable for `tabulated-list-entries'.
PKG has the form (PKG-DESC . STATUS).
Return (PKG-DESC [STAR NAME VERSION STATUS DOC])."
  (let* ((pkg-desc (car pkg))
         (status  (cdr pkg))
         (face (or (cdr (assoc-string status status-face-alist))
                   'font-lock-warning-face))
         (url (-package-homepage pkg-desc))
         (name (symbol-name (package-desc-name pkg-desc)))
         (name-length (length name))
         (button-length (length homepage-button-string)))
    (-incf status)
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

(defun -count-print (pkg)
  "Return counts of PKG as a package-desc list."
  (append
   (when (and display-star-count (listp -star-count))
     (list (-package-star-count pkg)))
   (when (and display-download-count (listp -download-count))
     (list (-package-download-count pkg)))))

(defun -package-download-count (pkg)
  "Return propertized string with the download count of PKG."
  (let ((c (cdr-safe (assoc pkg -download-count))))
    (propertize
     (if (numberp c)
         (if (> c 999) (format "%sK" (truncate c 1000)) (format "%s" c))
       " ")
     'face 'paradox-download-face
     'value (or c 0))))

(defun menu-view-commit-list (pkg)
  "Visit the commit list of package named PKG.
PKG is a symbol. Interactively it is the package under point."
  (interactive '(nil))
  (let ((repo (cdr (assoc (-get-or-return-package pkg)
                          -package-repo-list))))
    (if repo
        (with-selected-window
            (display-buffer (get-buffer-create -commit-list-buffer))
          (commit-list-mode)
          (setq -repo repo)
          (-commit-list-update-entries)
          (tabulated-list-print))
      (message "Package %s is not a GitHub repo." pkg))))

(defun menu-visit-homepage (pkg)
  "Visit the homepage of package named PKG.
PKG is a symbol. Interactively it is the package under point."
  (interactive '(nil))
  (let ((url (-package-homepage
              (-get-or-return-package pkg))))
    (if (stringp url)
        (browse-url url)
      (message "Package %s has no homepage."
               (propertize (symbol-name pkg)
                           'face 'font-lock-keyword-face)))))

(unless (-compat-p)
  (defun -package-homepage (pkg)
    "PKG can be the package-name symbol or a package-desc object."
    (let* ((object   (if (symbolp pkg) (cadr (assoc pkg package-archive-contents)) pkg))
           (name     (if (symbolp pkg) pkg (package-desc-name pkg)))
           (extras   (package-desc-extras object))
           (homepage (and (listp extras) (cdr-safe (assoc :url extras)))))
      (or homepage
          (and (setq extras (cdr (assoc name -package-repo-list)))
               (format "https://github.com/%s" extras)))))
  (defun -get-or-return-package (pkg)
    (if (or (markerp pkg) (null pkg))
        (if (derived-mode-p 'package-menu-mode)
            (package-desc-name (tabulated-list-get-id))
          (error "Not in Package Menu"))
      pkg)))

(defun -incf (status)
  "Increment the count for STATUS on `paradox--package-count'.
Also increments the count for \"total\"."
  (-inc-count status)
  (unless (string= status "obsolete")
    (-inc-count "total")))

(defun -inc-count (string)
  "Increment the cdr of (assoc-string STRING paradox--package-count)."
  (let ((cons (assoc-string string -package-count)))
    (setcdr cons (1+ (cdr cons)))))

(defun -entry-star-count (entry)
  "Get the star count of the package in ENTRY."
  (-package-star-count
   ;; The package symbol should be in the ID field, but that's not mandatory,
   (or (ignore-errors (elt (car entry) 1))
       ;; So we also try interning the package name.
       (intern (car (elt (cadr entry) 0))))))

(defvar -user-starred-list nil)

(defun -package-star-count (package)
  "Get the star count of PACKAGE."
  (let ((count (cdr (assoc package -star-count)))
        (repo (cdr-safe (assoc package -package-repo-list))))
    (propertize
     (format "%s" (or count ""))
     'face
     (if (and repo (assoc-string repo -user-starred-list))
         'paradox-starred-face
       'paradox-star-face))))

(defvar -column-index-star nil)
(defvar -column-index-download nil)

(defun -star-predicate (A B)
  "Non-nil t if star count of A is larget than B."
  (> (string-to-number (elt (cadr A) -column-index-star))
     (string-to-number (elt (cadr B) -column-index-star))))
(defun -download-predicate (A B)
  "Non-nil t if download count of A is larget than B."
  (> (get-text-property 0 'value (elt (cadr A) -column-index-download))
     (get-text-property 0 'value (elt (cadr B) -column-index-download))))

(defun -generate-menu (remember-pos packages &optional keywords)
  "Populate the Package Menu, without hacking into the header-format.
If REMEMBER-POS is non-nil, keep point on the same entry.
PACKAGES should be t, which means to display all known packages,
or a list of package names (symbols) to display.

With KEYWORDS given, only packages with those keywords are
shown."
  (mapc (lambda (x) (setf (cdr x) 0)) -package-count)
  (let ((desc-prefix (if (> lines-per-entry 1) " \n      " ""))
        (desc-suffix (make-string (max 0 (- lines-per-entry 2)) ?\n)))
    (menu--refresh packages keywords))
  (setq -current-filter
        (if keywords (mapconcat 'identity keywords ",")
          nil))
  (let ((idx (-column-index "Package")))
    (setcar (aref tabulated-list-format idx)
            (if keywords
                (concat "Package[" -current-filter "]")
              "Package")))
  (tabulated-list-print remember-pos)
  (tabulated-list-init-header)
  (-update-mode-line)
  (refresh-upgradeable-packages))

(if (-compat-p)
    (require 'paradox-compat)
  (defalias 'menu--refresh 'package-menu--refresh))

(defun -column-index (regexp)
  "Find the index of the column that matches REGEXP."
  (cl-position (format "\\`%s\\'" (regexp-quote regexp)) tabulated-list-format
            :test (lambda (x y) (string-match x (or (car-safe y) "")))))

(defun previous-entry (&optional n)
  "Move to previous entry, which might not be the previous line.
With prefix N, move to the N-th previous entry."
  (interactive "p")
  (next-entry (- n))
  (forward-line 0)
  (forward-button 1))

(defun next-entry (&optional n)
  "Move to next entry, which might not be the next line.
With prefix N, move to the N-th next entry."
  (interactive "p")
  (dotimes (it (abs n))
    (let ((d (cl-signum n)))
      (forward-line (if (> n 0) 1 0))
      (if (eobp) (forward-line -1))
      (forward-button d))))

(defun filter-upgrades ()
  "Show only upgradable packages."
  (interactive)
  (if (null -upgradeable-packages)
      (message "No packages have upgrades.")
    (package-show-package-list
     (mapcar 'car -upgradeable-packages))
    (setq -current-filter "Upgrade")))

(define-derived-mode menu-mode tabulated-list-mode "Paradox Menu"
  "Major mode for browsing a list of packages.
Letters do not insert themselves; instead, they are commands.
\\<paradox-menu-mode-map>
\\{paradox-menu-mode-map}"
  (hl-line-mode 1)
  (-update-mode-line)
  (when (-compat-p)
    (require 'paradox-compat)
    (setq tabulated-list-printer #'-print-entry-compat))
  (setq tabulated-list-format
        `[("Package" ,paradox-column-width-package package-menu--name-predicate)
          ("Version" ,paradox-column-width-version nil)
          ("Status" ,paradox-column-width-status package-menu--status-predicate)
          ,@(paradox--archive-format)
          ,@(paradox--count-format)
          ("Description" 0 nil)])
  (setq -column-index-star
        (-column-index -column-name-star))
  (setq -column-index-download
        (-column-index -column-name-download))
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Status" nil))
  ;; (add-hook 'tabulated-list-revert-hook 'package-menu--refresh nil t)
  (add-hook 'tabulated-list-revert-hook #'refresh-upgradeable-packages nil t)
  (add-hook 'tabulated-list-revert-hook #'-refresh-star-count nil t)
  (add-hook 'tabulated-list-revert-hook #'-update-mode-line nil t)
  (tabulated-list-init-header)
  ;; We need package-menu-mode to be our parent, otherwise some
  ;; commands throw errors. But we can't actually derive from it,
  ;; otherwise its initialization will screw up the header-format. So
  ;; we "patch" it like this.
  (put 'paradox-menu-mode 'derived-mode-parent 'package-menu-mode)
  (run-hooks 'package-menu-mode-hook))

(defun -count-format ()
  "List of star/download counts to be used as part of the entry."
  (remove
   nil
   (list
    (when display-star-count
      (list -column-name-star column-width-star
            #'-star-predicate :right-align t))
    (when display-download-count
      (list -column-name-download column-width-download
            #'-download-predicate :right-align t)))))

(defun paradox--archive-format ()
  "List containing archive to be used as part of the entry."
  (when (and (cdr package-archives)
             (null (paradox--compat-p)))
    (list (list "Archive"
                (apply 'max (mapcar 'length (mapcar 'car package-archives)))
                'package-menu--archive-predicate))))

(add-hook 'paradox-menu-mode-hook 'paradox-refresh-upgradeable-packages)

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

(defadvice package-menu-execute
    (around paradox-around-package-menu-execute-advice ())
  "Star/Unstar packages which were installed/deleted during `package-menu-execute'."
  (when (and (stringp paradox-github-token)
             (eq paradox-automatically-star 'unconfigured))
    (customize-save-variable
     'paradox-automatically-star
     (y-or-n-p "When you install new packages would you like them to be automatically starred?\n(They will be unstarred when you delete them) ")))
  (if (and (stringp paradox-github-token) paradox-automatically-star)
      (let ((before (paradox--repo-alist)) after)
        ad-do-it
        (setq after (paradox--repo-alist))
        (mapc #'paradox--star-repo
              (-difference (-difference after before) paradox--user-starred-list))
        (mapc #'paradox--unstar-repo
              (-intersection (-difference before after) paradox--user-starred-list))
        (package-menu--generate t t))
    ad-do-it))

(defun paradox--repo-alist ()
  "List of known repos."
  (cl-remove-duplicates
   (remove nil
           (--map (cdr-safe (assoc (car it) paradox--package-repo-list))
                  package-alist))))


;;; Github api stuff
(defmacro paradox--enforce-github-token (&rest forms)
  "If a token is defined, perform FORMS, otherwise ignore forms ask for it be defined."
  `(if (stringp paradox-github-token)
       (progn ,@forms)
     (setq paradox-github-token nil)
     (paradox--check-github-token)))

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

(defun paradox-star-all-installed-packages ()
  "Star all of your currently installed packages.
No questions asked."
  (interactive)
  (paradox--enforce-github-token
   (mapc (lambda (x) (paradox--star-package-safe (car-safe x))) package-alist)))

(defun paradox--star-package-safe (pkg &optional delete query)
  "Star PKG without throwing errors, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first."
  (let ((repo (cdr-safe (assoc pkg paradox--package-repo-list))))
    (when (and repo (not (assoc repo paradox--user-starred-list)))
      (paradox--star-repo repo delete query))))

(defun paradox--star-repo (repo &optional delete query)
  "Star REPO, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first.

Throws error if repo is malformed."
  (when (or (not query)
            (y-or-n-p (format "Really %sstar %s? "
                              (if delete "un" "") repo)))
    (paradox--github-action-star repo delete)
    (message "%starred %s." (if delete "Uns" "S") repo)
    (if delete
        (setq paradox--user-starred-list
              (remove repo paradox--user-starred-list))
      (add-to-list 'paradox--user-starred-list repo))))
(defun paradox--unstar-repo (repo &optional delete query)
  "Unstar REPO.
Calls (paradox--star-repo REPO (not DELETE) QUERY)."
  (paradox--star-repo repo (not delete) query))

(defun paradox--refresh-user-starred-list ()
  "Fetch the user's list of starred repos."
  (setq paradox--user-starred-list
        (paradox--github-action
         "user/starred?per_page=100" nil
         'paradox--full-name-reader)))

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

(defun paradox--github-action-star (repo &optional delete no-result)
  "Call `paradox--github-action' with \"user/starred/REPO\" as the action.
DELETE and NO-RESULT are passed on."
  (paradox--github-action (concat "user/starred/" repo)
                          (if (stringp delete) delete (if delete "DELETE" "PUT"))
                          (null no-result)))

(defun paradox--github-action (action &optional method reader max-pages)
  "Contact the github api performing ACTION with METHOD.
Default METHOD is \"GET\".

Action can be anything such as \"user/starred?per_page=100\". If
it's not a full url, it will be prepended with
\"https://api.github.com/\".

The api action might not work if `paradox-github-token' isn't
set. This function also handles the pagination used in github
results, results of each page are appended. Use MAX-PAGES to
limit the number of pages that are fetched.

Return value is always a list.
- If READER is nil, the result of the action is completely
  ignored (no pagination is performed on this case, making it
  much faster).
- Otherwise:
  - If the result was a 404, the function returns nil;
  - Otherwise, READER is called as a function with point right
    after the headers and should always return a list. As a
    special exception, if READER is t, it is equivalent to a
    function that returns (t)."
  ;; Make sure the token's configured.
  (unless (string-match "\\`https?://" action)
    (setq action (concat "https://api.github.com/" action)))
  ;; Make the request
  (message "Contacting %s" action)
  (let ((pages (if (boundp 'pages) (1+ pages) 1)) next)
    (append
     (with-temp-buffer
       (save-excursion
         (shell-command
          (if (stringp paradox-github-token)
              (format "curl -s -i -d \"\" -X %s -u %s:x-oauth-basic \"%s\" "
                      (or method "GET") paradox-github-token action)

            (format "curl -s -i -d \"\" -X %s \"%s\" "
                    (or method "GET") action)) t))
       (when reader
         (unless (search-forward " " nil t)
           (message "%s" (buffer-string))
           (error ""))
         ;; 204 means OK, but no content.
         (if (looking-at "204") '(t)
           ;; 404 is not found.
           (if (looking-at "404") nil
             ;; Anything else gets interpreted.
             (when (search-forward-regexp "^Link: .*<\\([^>]+\\)>; rel=\"next\"" nil t)
               (setq next (match-string-no-properties 1)))
             (search-forward-regexp "^?$")
             (skip-chars-forward "[:blank:]\n")
             (delete-region (point-min) (point))
             (unless (eobp) (if (eq reader t) t (funcall reader)))))))
     (when (and next (or (null max-pages) (< pages max-pages)))
       (paradox--github-action next method reader)))))

(defun paradox--check-github-token ()
  "Check that the user has either set or refused the github token.
If neither has happened, ask the user now whether he'd like to
configure or refuse the token."
  (if (stringp
       paradox-github-token) t
    (if paradox-github-token
        t
      (if (not (y-or-n-p "Would you like to set up GitHub integration?
This will allow you to star/unstar packages from the Package Menu. "))
          (customize-save-variable 'paradox-github-token t)
        (describe-variable 'paradox-github-token)
        (when (get-buffer "*Help*")
          (switch-to-buffer "*Help*")
          (delete-other-windows))
        (if (y-or-n-p "Follow the instructions on the `paradox-github-token' variable.
May I take you to the token generation page? ")
            (browse-url "https://github.com/settings/tokens/new"))
        (message "Once you're finished, simply call `paradox-list-packages' again.")
        nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Paradox Commit List Mode
(defun paradox--commit-tabulated-list (repo)
  "Return the tabulated list for REPO's commit list."
  (require 'json)
  (let ((feed (paradox--github-action (format "repos/%s/commits?per_page=100" repo)
                                      "GET" 'json-read 1)))
    (apply 'append (mapcar 'paradox--commit-print-info feed))))

(defun paradox--commit-print-info (x)
  "Parse json in X into a tabulated list entry."
  (let* ((commit (cdr (assoc 'commit x)))
         (date  (cdr (assoc 'date (cdr (assoc 'committer commit)))))
         (title (split-string (cdr (assoc 'message commit)) "[\n\r][ \t]*" t))
         (url   (cdr (assoc 'url commit)))
         (cc    (cdr (assoc 'comment_count commit))))
    (cons
     (list (cons (car title) x)
           (vector
            (propertize (format-time-string "%x" (date-to-time date))
                        'button t
                        'follow-link t
                        'action 'paradox-commit-list-visit-commit
                        'face 'link)
            (concat (if (> cc 0)
                        (propertize (format "(%s comments) " cc)
                                    'face 'font-lock-function-name-face)
                      "")
                    (or (car-safe title) ""))))
     (when (cdr title)
       (mapcar (lambda (m) (list (cons m x)
                            (vector "" m))) (cdr title))))))

(defun paradox--commit-list-update-entries ()
  "Update entries of current commit-list."
  (setq tabulated-list-entries
        (paradox--commit-tabulated-list paradox--repo)))

(defun paradox-commit-list-visit-commit (&optional ignore)
  "Visit this commit on GitHub.
IGNORE is ignored."
  (interactive)
  (when (derived-mode-p 'paradox-commit-list-mode)
    (browse-url
     (cdr (assoc 'html_url (tabulated-list-get-id))))))

(defun paradox-previous-commit (&optional n)
  "Move to previous commit, which might not be the previous line.
With prefix N, move to the N-th previous commit."
  (interactive "p")
  (paradox-next-commit (- n)))

(defun paradox-next-commit (&optional n)
  "Move to next commit, which might not be the next line.
With prefix N, move to the N-th next commit."
  (interactive "p")
  (dotimes (it (abs n))
    (let ((d (cl-signum n)))
      (forward-line d)
      (while (looking-at "  +")
        (forward-line d)))))

(define-derived-mode paradox-commit-list-mode
  tabulated-list-mode "Paradox Commit List"
  "Major mode for browsing a list of commits.
Letters do not insert themselves; instead, they are commands.
\\<paradox-commit-list-mode-map>
\\{paradox-commit-list-mode-map}"
  (hl-line-mode 1)
  (setq tabulated-list-format
        `[("Date" ,(length (format-time-string "%x" (current-time))) nil)
          ("Message" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'paradox--commit-list-update-entries)
  (tabulated-list-init-header))

(define-key paradox-commit-list-mode-map "" #'paradox-commit-list-visit-commit)
(define-key paradox-commit-list-mode-map "p" #'paradox-previous-commit)
(define-key paradox-commit-list-mode-map "n" #'paradox-next-commit)

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

