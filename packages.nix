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
  global-language-tools = with pkgs; [ gopls gotools rnix-lsp rustup ];
  kubernetes-tools = with pkgs;
    [ fluxcd kubectl kubernetes-helm kubeseal velero ]
    ++ (with pkgs; lib.optionals stdenv.isLinux [ kube3d ]);
  nix-tools = [ devenv.defaultPackage.${pkgs.stdenv.system} ];
  platform-tools = with pkgs; [ gh terraform ];
in container-tools ++ core-tools ++ global-language-tools ++ kubernetes-tools
++ nix-tools ++ platform-tools
