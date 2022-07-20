{ pkgs }:
with pkgs;
[
  awscli
  bat
  cacert
  coreutils
  curl
  direnv
  coreutils
  dive
  fluxcd
  fzf
  gdb
  gh
  git
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
  rnix-lsp
  rust-analyzer
  rustup
  terraform
  tmux
  tree
  velero
  wget
  xxd
  yubikey-manager
  yq
  zbar
  zstd
] ++ (with pkgs;
  lib.optionals stdenv.isLinux [ conmon gcc kube3d podman valgrind ])
