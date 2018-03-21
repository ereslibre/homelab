set fish_greeting

cat ~/.todo

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"
set -x GOPATH $HOME/.go
set -x CARGOPATH $HOME/.cargo

set PATH /sbin /usr/sbin $HOME/.bin $GOPATH/bin $CARGOPATH/bin $PATH

alias iosc="osc -A https://api.suse.de"
alias mutt="/usr/bin/mutt -F ~/.muttrc-gmail"
alias smutt="/usr/bin/mutt -F ~/.muttrc-suse"

if type -q keychain; and not set -q WINDOW_MANAGER
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
