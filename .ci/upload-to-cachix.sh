#!/bin/sh

set -eu
set -f # disable globbing
export IFS=' '

echo "Uploading paths" $OUT_PATHS
exec nix-shell -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/tarball/nixos-19.09 -p cachix --run 'cachix push ihaskell $OUT_PATHS'
