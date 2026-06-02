{
  pkgs,
  nix-ai-tools,
  ...
}: let
  ai-tools = nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system};
in {
  environment.systemPackages =
    (with pkgs; [
      lm_sensors
      ltrace
      man-pages
      man-pages-posix
    ])
    ++ (with ai-tools; [ccusage]);
}
