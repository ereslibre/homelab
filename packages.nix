{ pkgs }:
with pkgs;
[
  awscli
  bat
  cacert
  coreutils
  cosign
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
  stack
  terraform
  tmux
  tree
  velero
  wget
  yubikey-manager
  yq
  zbar
  zstd
] ++ (with pkgs;
  lib.optionals stdenv.isLinux [ conmon gcc kube3d podman trivy valgrind ])
