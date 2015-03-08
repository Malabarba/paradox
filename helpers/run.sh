#!/usr/bin/bash

[ -z "$EMACS" ] && echo "NO EMACS!!" && return

# Paradox
cd ~/.melpa &&
    git pull &&
    cd ~/.paradox/helpers &&
    git checkout data &&
    git pull &&
    $EMACS --batch -Q -L . -L .. -l paradox-counter.el -f paradox-generate-star-count &&
    git add .. &&
    git commit -m "$(date)" &&
    git push -v origin data:refs/heads/data &&
    
    
