{ username, pkgs }:
let macFiles = import ./mac.nix { inherit username pkgs; };
in {
  ".emacs.d" = {
    source = ./assets/emacs/emacs.d;
    recursive = true;
  };
  ".gitconfig".source = ./assets/git/config;
  ".gitignore".source = ./assets/git/gitignore;
  ".ssh/config".text = import ./assets/ssh/config.nix { inherit username; };
  ".tmux.conf".source = ./assets/tmux/config;
} // macFiles
