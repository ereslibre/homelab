#!/usr/bin/env bash

if [[ "$(uname -o)" == "Darwin" ]]; then
    sudo nix --extra-experimental-features nix-command --extra-experimental-features flakes run nix-darwin -- switch --flake .#"$@"
else
    sudo nixos-rebuild --flake .#"$@" switch
fi
