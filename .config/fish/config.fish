set fish_greeting

cat ~/.todo

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"
set -x GOPATH $HOME/.go
set -x CARGOPATH $HOME/.cargo

set PATH /sbin /usr/sbin $HOME/.bin $GOPATH/bin $CARGOPATH/bin $PATH
