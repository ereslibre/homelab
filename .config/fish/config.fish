set fish_greeting

cat ~/.todo

set -x TERMINAL tilix
set -x EDITOR "emacsclient -t"
set -x GIT_EDITOR "emacsclient -t"
set -x GOPATH $HOME/projects/go
set -x CARGOPATH $HOME/.cargo
set -x SSH_AUTH_SOCK (gnome-keyring-daemon --start | awk -F= '{print  $2}')
# Set default bazel directory
set -x TEST_TMPDIR "$HOME/.cache/bazel-local"

set PATH /sbin /usr/sbin $HOME/.bin $GOPATH/bin $CARGOPATH/bin /usr/local/kubebuilder/bin $PATH

alias emacs="emacsclient -t"
alias bs="docker run --rm -v ~/.config/osc/oscrc:/root/.config/osc/oscrc -v ~/projects/suse/ibs:/root/ibs -v ~/projects/suse/obs:/root/obs -it ereslibre/buildservice"

if type -q keychain; and not set -q WINDOW_MANAGER
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end
