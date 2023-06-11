#!/usr/bin/env bash

set -x

SOCKPATH=""
if [ -S ~/.emacs.d/emacs.sock ]; then
    SOCKPATH="-f ~/.emacs.d/emacs.sock"
elif [ -S /run/user/$(id -u)/emacs/server ]; then
    SOCKPATH="-f /run/user/$(id -u)/emacs/server"
fi
emacsclient -s $SOCKPATH --eval '(kill-emacs)'
rm -rf assets/emacs/emacs.d/elpa
mkdir -p assets/emacs/emacs.d/elpa
make
~/.nix-profile/bin/emacs --batch --debug-init --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el -f package-refresh-contents
~/.nix-profile/bin/emacs --batch --debug-init --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el -f kill-emacs
cp -r ~/.emacs.d/elpa assets/emacs/emacs.d
rm -rf ~/.emacs.d/elpa
git add assets/emacs
make
