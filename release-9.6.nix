let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/02c6061679c99546ccff9ca241025134411e13ad";
    sha256 = "sha256:03cwv27x1xl9fm2dzdgzm1lz9qimn3w10v8pb7wd4ld459jir8pb";
  };
  ghcVersion = "ghc963";
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ${ghcVersion} = sup.haskell.packages.${ghcVersion}.override {
          overrides = self: super: {
            ghc-syntax-highlighter = let
              src = sel.fetchFromGitHub {
                owner = "mrkkrp";
                repo = "ghc-syntax-highlighter";
                rev = "71ff751eaa6034d4aef254d6bc5a8be4f6595344";
                sha256 = "14yahxi4pnjbvcd9r843kn7b36jsjaixd99jswsrh9n8xd59c2f1";
              };
              in
                self.callCabal2nix "ghc-syntax-highlighter" src {};
          };
        };
      };
    };
  };
in
{ compiler ? ghcVersion
, nixpkgs ? import nixpkgs-src { overlays = [ overlay ]; }
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler nixpkgs packages pythonPackages rtsopts systemPackages; }
