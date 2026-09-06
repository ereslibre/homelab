{
  pkgs,
  llm-agents,
  revdiff,
  ...
}: let
  ai-tools = llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
in {
  environment.systemPackages =
    (with pkgs; [
      lm_sensors
      ltrace
      man-pages
      man-pages-posix
    ])
    ++ (with ai-tools; [ccusage])
    # Not in nixpkgs; comes from the `revdiff` flake input.
    ++ [revdiff.packages.${pkgs.stdenv.hostPlatform.system}.default];
}
