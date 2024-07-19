{pkgs, ...}: let
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
    just
    mosh
    mtr
    gnupg-pkcs11-scd
    otpauth
    ripgrep
    rlwrap
    tree
    watch
    wget
    xxd
    zstd
  ];
  global-language-tools = with pkgs; [gopls gotools rustup];
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals pkgs.stdenv.isLinux [kind kube3d]));
  platform-tools = with pkgs; [gh];
in {
  home.packages =
    container-tools
    ++ core-tools
    ++ global-language-tools
    ++ kubernetes-tools
    ++ platform-tools;
}
