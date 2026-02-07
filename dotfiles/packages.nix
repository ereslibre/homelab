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
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals pkgs.stdenv.isLinux [k3d kind]));
  platform-tools = with pkgs; [gh];
in {
  home.packages =
    container-tools
    ++ core-tools
    ++ global-language-tools
    ++ kubernetes-tools
    ++ platform-tools;
}
