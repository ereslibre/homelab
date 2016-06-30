set fish_greeting

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"

if [ -x /usr/bin/keychain ]
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
