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
  global-language-tools = with pkgs; [gopls gotools rnix-lsp rustup];
  kubernetes-tools = with pkgs; ([fluxcd kubectl kubernetes-helm kubeseal velero] ++ (lib.optionals stdenv.isLinux [kind kube3d]));
  nix-tools = [devenv.packages.${pkgs.stdenv.system}.default];
  platform-tools = with pkgs; [gh opentofu];
in {
  home.packages =
    container-tools
    ++ core-tools
    ++ global-language-tools
    ++ kubernetes-tools
    ++ nix-tools
    ++ platform-tools;
}
