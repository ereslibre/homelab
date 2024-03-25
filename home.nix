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
}: let
  systemMatchesPredicate = system: predicate:
    lib.systems.inspect.predicates."${predicate}" (lib.systems.parse.mkSystemFromString "${system}");
in {
  imports =
    [
      (import ./dotfiles.nix {inherit username profile;})
      ./fonts.nix
      (import ./packages.nix {inherit devenv;})
      (import ./programs.nix {
        inherit mainlyRemote;
        isDarwin = systemMatchesPredicate system "isDarwin";
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
      [(import ./mac.nix {inherit username;})]
    );

  home = {inherit username homeDirectory stateVersion;};
}
