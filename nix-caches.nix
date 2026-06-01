# Binary caches that every machine's `nix.settings` marks as trusted, so
# `nix build` never prompts to allow the flake's `extra-substituters`.
#
# Keep in sync with flake.nix's `nixConfig`, which must list the same values
# literally (nixConfig is read in a restricted mode that rejects `import`).
{
  substituters = [
    "https://cache.flox.dev"
    "https://cache.nixos-cuda.org"
    "https://cache.numtide.com"
    "https://nix-community.cachix.org"
  ];
  trustedPublicKeys = [
    "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
    "cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="
    "niks3.numtide.com-1:DTx8wZduET09hRmMtKdQDxNNthLQETkc/yaX7M4qK0g="
    "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
  ];
}
