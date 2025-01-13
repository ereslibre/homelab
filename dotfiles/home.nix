{
  home-manager,
  homeDirectory,
  mainlyRemote,
  profile,
  stateVersion,
  system,
  username,
}: {
  config,
  pkgs,
  lib,
  ...
}: let
  systemMatchesPredicate = system: predicate:
    lib.systems.inspect.predicates."${predicate}" (lib.systems.parse.mkSystemFromString "${system}");
in {
  imports =
    [
      (import ./dotfiles.nix {inherit profile;})
      ./fonts.nix
      ./packages.nix
      (import ./programs.nix {
        inherit username mainlyRemote profile;
        isDarwin = systemMatchesPredicate system "isDarwin";
        isLinux = systemMatchesPredicate system "isLinux";
      })
    ]
    ++ (
      lib.optionals (systemMatchesPredicate system "isLinux")
      [
        (import ./node.nix {inherit home-manager;})
        ./systemd.nix
      ]
    )
    ++ (
      lib.optionals (systemMatchesPredicate system "isDarwin")
      [
        ./mac.nix
      ]
    );

  home = {
    inherit username homeDirectory stateVersion;
  };
}
