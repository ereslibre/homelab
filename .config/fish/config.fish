set fish_greeting

echo
cat ~/.todo
echo

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"

set PATH $HOME/bin $PATH

if [ -x /usr/bin/keychain ]
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
