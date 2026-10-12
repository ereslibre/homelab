{
  pkgs,
  llm-agents,
  ...
}: let
  ai-tools = llm-agents.packages.${pkgs.stdenv.hostPlatform.system};
in {
  environment.systemPackages =
    (with pkgs; [
      lm_sensors
      # Not in the binary cache for aarch64 at the current pin, and its
      # test suite drives ptrace, which qemu-user (how hulk builds the
      # Pis) cannot emulate. x86_64 keeps the cached, tested build.
      (
        if stdenv.hostPlatform.isAarch64
        then ltrace.overrideAttrs {doCheck = false;}
        else ltrace
      )
      man-pages
      man-pages-posix
    ])
    ++ (with ai-tools; [ccusage]);
}
