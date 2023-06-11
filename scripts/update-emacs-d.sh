#!/usr/bin/env bash

set -x

SOCKPATH=""
if [ -S $HOME/.emacs.d/emacs.sock ]; then
    SOCKPATH="$HOME/.emacs.d/emacs.sock"
elif [ -S /run/user/$(id -u)/emacs/server ]; then
    SOCKPATH="/run/user/$(id -u)/emacs/server"
else
    echo "could not find emacs server socket"
    exit 1
fi
emacsclient -s "$SOCKPATH" --eval '(kill-emacs)'
rm -rf assets/emacs/emacs.d/elpa
mkdir -p assets/emacs/emacs.d/elpa
make
~/.nix-profile/bin/emacs --batch --debug-init --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el -f package-refresh-contents
~/.nix-profile/bin/emacs --batch --debug-init --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el -f kill-emacs
cp -r ~/.emacs.d/elpa assets/emacs/emacs.d
rm -rf ~/.emacs.d/elpa
git add assets/emacs
make
