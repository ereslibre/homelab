{
  devenv,
  pkgs,
  ...
}: let
  container-tools = with pkgs; ([dive reg regctl] ++ lib.optionals stdenv.isLinux [distrobox]);
  core-tools = with pkgs; [
    alacritty
    binutils
    coreutils
    curl
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
    watch
    wget
    xxd
    yubikey-manager
    zstd
  ];
  global-language-tools = with pkgs; [gopls gotools rnix-lsp rustup];
  infra-tools = with pkgs; [ipmitool];
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals stdenv.isLinux [kind kube3d]));
  nix-tools = [devenv.packages.${pkgs.stdenv.system}.default];
  platform-tools = with pkgs; [gh terraform];
in {
  home.packages =
    container-tools
    ++ core-tools
    ++ global-language-tools
    ++ infra-tools
    ++ kubernetes-tools
    ++ nix-tools
    ++ platform-tools;
}
