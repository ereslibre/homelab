{
  username,
  profile,
  pkgs,
  ...
}: {
  home.file.".config/nix/nix.conf".source = ./assets/nix/nix.conf;
  home.file.".emacs.d" = {
    source = ./assets/emacs/emacs.d;
    recursive = true;
  };
  home.file.".gitconfig.vmware".source = ./assets/git/config-vmware;
  home.file.".gitignore".source = ./assets/git/gitignore;
  home.file.".gitconfig".source =
    if profile == "personal"
    then ./assets/git/full-config
    else ./assets/git/full-config-vmware;
  home.file.".ssh/config".text = import ./assets/ssh/config.nix {inherit username;};
}
