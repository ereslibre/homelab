{
  username,
  homeDirectory,
  stateVersion,
  profile,
  system,
  mainlyRemote,
  devenv,
  home-manager,
}: {
  pkgs,
  lib,
  ...
}: {
  imports =
    [
      (import ./dotfiles.nix {inherit username profile;})
      (import ./packages.nix {inherit devenv;})
      (import ./programs.nix {inherit mainlyRemote;})
    ]
    ++ (
      lib.optionals (system == "x86_64-linux")
      [
        (import ./node.nix {inherit home-manager;})
        (import ./systemd.nix)
      ]
    )
    ++ (
      lib.optionals (system == "x86_64-darwin")
      [(import ./mac.nix {inherit username;})]
    );

  home = {inherit username homeDirectory stateVersion;};
}
