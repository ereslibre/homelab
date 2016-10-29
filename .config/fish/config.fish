set fish_greeting

cat ~/.todo

set -x EDITOR "emacs -nw -q"
set -x GIT_EDITOR "emacs -nw -q"

set -x CC "ccache gcc"
set -x CXX "ccache g++"

set PATH $HOME/bin /usr/lib/ccache/bin $PATH
set -x GOPATH $HOME/projects/go-workspace

set -x KUBERNETES_PROVIDER "vagrant"
set -x VAGRANT_DEFAULT_PROVIDER "virtualbox"

if [ -x /usr/bin/keychain ]
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
