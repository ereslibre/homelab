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
      (import ./packages.nix {inherit devenv;})
      (import ./programs.nix {inherit mainlyRemote;})
    ]
    ++ (
      lib.optionals (systemMatchesPredicate system "isLinux")
      [
        (import ./node.nix {inherit home-manager;})
        (import ./systemd.nix)
      ]
    )
    ++ (
      lib.optionals (systemMatchesPredicate system "isDarwin")
      [(import ./mac.nix {inherit username;})]
    );

  home = {inherit username homeDirectory stateVersion;};
}
