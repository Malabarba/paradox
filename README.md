Paradox
=======

Project for modernizing Emacs' Package Menu. With package ratings,
usage statistics, customizability, and more.

### Before ###
![Regular Package Menu](https://raw.github.com/Bruce-Connor/paradox/master/before.png)

### After ###
![Paradox Package Menu](https://raw.github.com/Bruce-Connor/paradox/master/after.png)  
*These screenshots use smart-mode-line, but a similar effect is obtained with the regular mode-line.*

Usage
===

To install it, open the file and call `M-x package-install-from-buffer`.

To use it, simply call `M-x paradox-list-packages` (instead of the regular `list-packages`).

## Current Features ##

* Display number of GitHub Stars the package has (to the left of the
  description).
* Shortcuts for package filtering:
    * `f r` filters by regexp (`occur`);
    * `f u` filters by upgrades;
    * `f k` filters by keyword.
* `hl-line-mode` enabled by default.
* Display useful information on the mode-line and cleanup a bunch of
  useless stuff (customizable).
* Customization! Just call `M-x paradox-customize` to see what you can
  do.
    * Customize column widths.
    * Customize faces (`paradox-star-face`,
      `paradox-status-face-alist` and `paradox-archive-face`).
    * Customize local variables.


## Planned Features ##

* Star and unstar packages from within the Package Menu.

## Known Bugs ##

* On some cases there's an annoying gnutls error message after downloading the star counts `gnutls.c: [0] (Emacs) fatal error: The TLS connection was non-properly terminated.`.  
  If anyone knows how to fix it, I'm all ears.

