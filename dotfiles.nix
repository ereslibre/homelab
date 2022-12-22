{ username, homeDirectory, profile, pkgs }:
let macFiles = import ./mac.nix { inherit username pkgs; };
in {
  ".emacs.d" = {
    source = ./assets/emacs/emacs.d;
    recursive = true;
  };
  ".gitconfig".text = (if profile == "personal" then
    import ./assets/git/full-config
  else
    import ./assets/git/full-config-work) { inherit homeDirectory; };
  ".gitconfig.vmware".source = ./assets/git/config-vmware;
  ".gitconfig.shared".source = ./assets/git/shared-config;
  ".gitignore".source = ./assets/git/gitignore;
  ".ssh/config".text = import ./assets/ssh/config.nix { inherit username; };
  ".tmux.conf".source = ./assets/tmux/config;
} // macFiles
