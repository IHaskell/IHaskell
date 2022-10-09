let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/bbcc162a42be96579ce25dd67b93f7cfad8b9f5c";
    sha256 = "0v1n777y7kaxbki4bcbw40iqj9xpkwzm2szqqcqc212w1hm1j5k2";
  };
in
{ compiler ? "ghc942"
, nixpkgs ? import nixpkgs-src {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler nixpkgs packages pythonPackages rtsopts systemPackages; }
