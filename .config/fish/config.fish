alias k=kubectl

set -x EDITOR "emacsclient -t"
set -x GO111MODULE "on"
set -x GOPATH "$HOME/projects/go"
set -x PATH $PATH "/opt/homebrew/bin" "$HOME/.cargo/bin" "$HOME/.bin" "$HOME/.zig" "$GOPATH/bin" "/usr/local/bin"
set fish_greeting

fenv source ~/.bashrc
fenv source ~/.nix-profile/etc/profile.d/nix.sh
