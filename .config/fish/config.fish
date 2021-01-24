alias emacs='emacsclient -t'
alias ip='ip -color=auto'
alias k=kubectl

set -x GO111MODULE 'on'
set -x GOPATH "$HOME/projects/go"
set -x XDG_RUNTIME_DIR "/run/user/(id -u)"
set -x PATH $PATH "$GOPATH/bin" "/usr/local/kubebuilder/bin"

set fish_greeting

fenv source ~/.bashrc
fenv source ~/.nix-profile/etc/profile.d/nix.sh
