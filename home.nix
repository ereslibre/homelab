{
  username,
  homeDirectory,
  stateVersion,
  pkgs,
  nixpkgs,
  profile,
  mainlyRemote,
  devenv,
  home-manager,
  ...
}: {
  imports =
    [
      (import ./dotfiles.nix {inherit username profile pkgs;})
      (import ./packages.nix {inherit devenv pkgs;})
      (import ./programs.nix {inherit mainlyRemote;})
    ]
    ++ nixpkgs.lib.optionals pkgs.stdenv.isDarwin [
      (import ./mac.nix {inherit username pkgs;})
    ]
    ++ nixpkgs.lib.optionals pkgs.stdenv.isLinux [
      (import ./node.nix {inherit home-manager pkgs;})
      ./systemd.nix
    ];

  home = {inherit username homeDirectory stateVersion;};
}
