alias diff='diff --color=auto'
alias emacs='emacsclient -t'
alias grep='grep --color=auto'
alias ip='ip -color=auto'
alias k=kubectl
alias ls='ls --color=auto'

set -x EDITOR "emacsclient -t"
set -x GO111MODULE "on"
set -x GOPATH "$HOME/projects/go"
set -x XDG_RUNTIME_DIR /run/user/(id -u)
set -x PATH $PATH "$HOME/.bin" "$HOME/.zig" "$GOPATH/bin" /home/ereslibre/.local/bin "/usr/local/kubebuilder/bin" "/usr/local/bin"
set -x GPG_TTY (tty)
set fish_greeting

fenv source ~/.bashrc
fenv source ~/.nix-profile/etc/profile.d/nix.sh

if not pgrep -f ssh-agent &> /dev/null
   set -e SSH_AUTH_SOCK
   eval (ssh-agent -c) &> /dev/null
end

if not pgrep -f gpg-agent &> /dev/null
   set -e GPG_AGENT_INFO
   eval (gpg-agent -c --daemon) &> /dev/null
end

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/ereslibre/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/ereslibre/.ghcup/bin $PATH
