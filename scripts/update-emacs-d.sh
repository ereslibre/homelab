#!/usr/bin/env bash

set -x

emacsclient --eval '(kill-emacs)'
rm -rf dotfiles/assets/emacs/emacs.d/elpa
mkdir -p dotfiles/assets/emacs/emacs.d/elpa
just
~/.nix-profile/bin/emacs --batch --debug-init --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el --load ~/.emacs.d/custom.el -f package-refresh-contents
~/.nix-profile/bin/emacs --batch --debug-init --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el --load ~/.emacs.d/custom.el -f kill-emacs
cp -r ~/.emacs.d/elpa dotfiles/assets/emacs/emacs.d
rm -rf ~/.emacs.d/elpa
git add dotfiles/assets/emacs
just
