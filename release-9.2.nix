{ compiler ? "ghc925"
, nixpkgs ? import <nixpkgs> {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler nixpkgs packages pythonPackages rtsopts systemPackages; }
