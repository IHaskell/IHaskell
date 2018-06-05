args@{ compiler ? "ghc822", nixpkgs ? import <nixpkgs> {}, packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:
  import (./release.nix) args
