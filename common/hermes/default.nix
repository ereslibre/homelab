{nix-ai-tools}: {pkgs, ...}: {
  environment.systemPackages = [
    nix-ai-tools.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent
  ];
}
