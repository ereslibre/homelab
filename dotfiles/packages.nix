{
  nix-ai-tools,
  pkgs,
  ...
}: let
  ai-tools = with nix-ai-tools.packages.${pkgs.system}; [
    claude-code
    codex
    copilot-cli
    gemini-cli
    qwen-code
  ];
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
  global-language-tools = with pkgs; [gopls gotools nodejs pnpm rustup];
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals pkgs.stdenv.isLinux [kind kube3d]));
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
