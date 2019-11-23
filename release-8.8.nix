{ compiler ? "ghc881"
, jupyterlabAppDir ? null
, nixpkgs ? import <nixpkgs> {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler jupyterlabAppDir nixpkgs packages pythonPackages rtsopts systemPackages; }
