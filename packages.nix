{ pkgs, pkgs-main }:
with pkgs;
[
  awscli
  bat
  binutils
  cacert
  coreutils
  curl
  direnv
  coreutils
  dive
  file
  fluxcd
  fzf
  gdb
  gh
  git
  gnumake
  gnupg
  gnupg-pkcs11-scd
  go
  gopls
  gotools
  kubernetes-helm
  htop
  jq
  just
  keychain
  kubectl
  kubeseal
  mtr
  otpauth
  reg
  ripgrep
  rlwrap
  rnix-lsp
  rust-analyzer
  rustup
  terraform
  tmux
  tree
  velero
  wget
  xxd
  zstd
] ++ (with pkgs; lib.optionals stdenv.isLinux [ kube3d valgrind ])
++ (with pkgs; lib.optionals (stdenv.system != "aarch64-darwin") [ yq zbar ])
++ (with pkgs-main;
  [
    # Consume from release-22.11 when available. This resolves some
    # derivation deps being marked as broken for aarch64-darwin on
    # release-22.05
    yubikey-manager
  ])
