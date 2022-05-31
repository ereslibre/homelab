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
  nixfmt
  nix-linter
  otpauth
  podman
  reg
  ripgrep
  rnix-lsp
  rust-analyzer
  rustup
  stack
  terraform
  tmux
  tree
  trivy
  velero
  wget
  yubikey-manager
  yq
  zstd
] ++ (with pkgs;
  lib.optionals stdenv.isLinux [ conmon gcc kube3d open-policy-agent valgrind ])
++ (with pkgs;
  lib.optionals (!stdenv.isAarch64) [
    # Qt does not build in aarch64
    zbar
  ])
