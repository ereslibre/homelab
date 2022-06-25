{ pkgs }:
with pkgs;
[
  awscli
  bat
  cabal2nix
  cacert
  cachix
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
  git-chglog
  gitRepo
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
  zstd
] ++ (with pkgs;
  lib.optionals stdenv.isLinux [
    conmon
    gcc
    kube3d
    open-policy-agent
    podman
    trivy
    valgrind
  ]) ++ (with pkgs;
    lib.optionals (!stdenv.isAarch64) [
      # Qt does not build in aarch64
      zbar
    ])
