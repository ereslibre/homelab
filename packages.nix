{ devenv, pkgs }:
let
  container-tools = with pkgs; [ distrobox dive reg ];
  core-tools = with pkgs; [
    bat
    binutils
    coreutils
    curl
    diffutils
    direnv
    file
    fzf
    git
    gnumake
    gnupg
    gnupg-pkcs11-scd
    htop
    jq
    just
    keychain
    mtr
    otpauth
    ripgrep
    rlwrap
    tree
    wget
    xxd
    yubikey-manager
    zstd
  ];
  global-language-tools = with pkgs; [
    gopls
    gotools
    rnix-lsp
    rust-analyzer
    rustup
  ];
  platform-tools = with pkgs; [ gh terraform ];
  kubernetes-tools = with pkgs;
    [ fluxcd kubectl kubernetes-helm kubeseal velero ]
    ++ (with pkgs; lib.optionals stdenv.isLinux [ kube3d ]);
in container-tools ++ core-tools ++ global-language-tools ++ platform-tools
++ kubernetes-tools
