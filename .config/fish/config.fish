set fish_greeting

set -x TERMINAL tilix
set -x EDITOR "emacsclient -t"
set -x GIT_EDITOR "emacsclient -t"

alias k="kubectl"
alias emacs="emacsclient -t"

if type -q keychain; and not set -q WINDOW_MANAGER
    keychain --nogui $HOME/.ssh/id_rsa ^/dev/null
    source $HOME/.keychain/(hostname)-fish
end

eval (~/.config/fish/misc/nix_fish_env.sh) 2>/dev/null

set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f $GHCUP_INSTALL_BASE_PREFIX/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin $GHCUP_INSTALL_BASE_PREFIX/.ghcup/bin $PATH
