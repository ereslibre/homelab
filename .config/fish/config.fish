set fish_greeting

cat ~/.todo

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"
set -x GOPATH $HOME/.go

set PATH /sbin /usr/sbin $HOME/.bin $GOPATH/bin $PATH

if [ -x /usr/bin/keychain ]
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
