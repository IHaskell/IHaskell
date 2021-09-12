let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/4869c88626382a01ed59b161afe99d9e164bd8a1";
    sha256 = "03ncnw0p28j5jsp5dh66jl003x81jhkh2larwkq3fvsbkai4x0kn";
  };
in
{ compiler ? "ghc901"
, nixpkgs ? import nixpkgs-src {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler nixpkgs packages pythonPackages rtsopts systemPackages; }
