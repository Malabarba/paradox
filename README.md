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

To install it, call `M-x package-install RET paradox`.

To use it, simply call `M-x paradox-list-packages` (instead of the
regular `list-packages`).  
This will give you most features out of the box. If you want to be
able to star packages as well, just configure the
`paradox-github-token` variable then call `paradox-list-packages`
again.

If you'd like to stop using Paradox, you may call `paradox-disable`
and go back to using the regular `list-packages`.

## Current Features ##

### Package Ratings ###

The first feature you should know about is our integration with
**GitHub Stars**, which works as *rough package rating* system.  
That is, Paradox package menu will:

1. Display the number of GitHub Stars each package has (assuming it's
   in a github repo, of course);
2. Automatically star packages you install, and unstar packages you delete;
3. Let you star and unstar packages by hitting the `s` key;
4. Let you star all packages you have installed with `M-x paradox-star-all-installed-packages`.

Item **1.** will work out of the box, the other items obviously
require a github account (Paradox will help you generate a token the
first time you call `paradox-list-packages`).
  
### Several Improvements ###

Other features include many small improvements to the package menu
itself and also work out of the box.

* Shortcuts for package filtering:
    * `f r` filters by regexp (`occur`);
    * `f u` display only packages with upgrades;
    * `f k` filters by keyword (emacs 24.4 only).
* `hl-line-mode` enabled by default.
* Display useful information on the mode-line and cleanup a bunch of
  useless stuff (customizable).
* Customization! Just call `M-x paradox-customize` to see what you can
  do.
    * Customize column widths.
    * Customize faces (`paradox-star-face`,
      `paradox-status-face-alist` and `paradox-archive-face`).
    * Customize local variables.


## Known Bugs ##

* On some cases there's an annoying gnutls error message after downloading the star counts `gnutls.c: [0] (Emacs) fatal error: The TLS connection was non-properly terminated.`.  
  If anyone knows how to fix it, I'm all ears.

