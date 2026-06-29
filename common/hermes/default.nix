{llm-agents}: {pkgs, ...}: {
  environment.systemPackages = [
    llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.hermes-agent
  ];
}
