{aiTools}: {
  llm-agents,
  pkgs,
  lib,
  ...
}: let
  ai-tools = lib.optionals aiTools (builtins.map (pkg:
    pkg.overrideAttrs (old: {
      doCheck = false;
      doInstallCheck = false;
    })) (
    with llm-agents.packages.${pkgs.stdenv.hostPlatform.system}; [
      claude-code
      codex
      copilot-cli
      cursor-agent
      # gemini-cli
      goose-cli
      hermes-agent
      opencode
      pi
      skills
      tuicr
      qwen-code
    ]
  ));
  container-tools = with pkgs; ([dive reg regctl] ++ lib.optionals pkgs.stdenv.isLinux [distrobox]);
  core-tools = with pkgs; [
    binutils
    coreutils
    curl
    devenv
    diffutils
    dig
    file
    gnumake
    gnupg
    gnupg-pkcs11-scd
    just
    mosh
    mtr
    otpauth
    ripgrep
    rlwrap
    tree
    unixtools.netstat
    watch
    wget
    xxd
    zstd
  ];
  # rustup ships its own `rust-analyzer` proxy shim, but it only works if the
  # active toolchain has the rust-analyzer component installed (mutable ~/.rustup
  # state, not captured here) — otherwise it recurses on PATH and dies. Pull the
  # standalone nixpkgs rust-analyzer instead and give it hiPrio so it wins the
  # bin/rust-analyzer collision against rustup's shim. cargo/rustc still come
  # from rustup; project-local toolchains (devenv/.envrc) still override via PATH.
  global-language-tools = with pkgs; [gopls nodejs pnpm rustup (lib.hiPrio rust-analyzer)];
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals pkgs.stdenv.isLinux [k3d kind]));
  platform-tools = with pkgs; [gh];
in {
  home.packages =
    ai-tools
    ++ container-tools
    ++ core-tools
    ++ global-language-tools
    ++ kubernetes-tools
    ++ platform-tools;
}
