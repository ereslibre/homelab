set fish_greeting

cat ~/.todo

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"
set -x GOPATH $HOME/.go

set PATH $HOME/.bin $GOPATH/bin $HOME/.gem/ruby/2.3.0/bin $PATH

alias dc docker-compose
alias dm docker-machine

if [ -x /usr/bin/keychain ]
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
