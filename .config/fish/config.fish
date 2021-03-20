alias diff='diff --color=auto'
alias emacs='emacsclient -t'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias k=kubectl
alias ls='ls --color=auto'

set -x GO111MODULE 'on'
set -x GOPATH "$HOME/projects/go"
set -x XDG_RUNTIME_DIR "/run/user/(id -u)"
set -x PATH $PATH "$HOME/.bin" "$GOPATH/bin" "/usr/local/kubebuilder/bin"

set fish_greeting

fenv source ~/.bashrc
fenv source ~/.nix-profile/etc/profile.d/nix.sh

set -e SSH_AUTH_SOCK
set -U -x SSH_AUTH_SOCK (gpgconf --list-dirs agent-ssh-socket)
if not pgrep -f gpg-agent &> /dev/null
   gpg-agent -c --daemon --enable-ssh-support
end

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/ereslibre/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/ereslibre/.ghcup/bin $PATH
