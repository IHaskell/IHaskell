let
  overlay = sel: sup: {
    haskell = sup.haskell // {
      packages = sup.haskell.packages // {
        ghc862 = sup.haskell.packages.ghc862.override {
          overrides = self: super: {
            base-orphans           = self.callHackage "base-orphans" "0.8" {};
            contravariant          = self.callHackage "contravariant" "1.5" {};
            doctest                = sel.haskell.lib.dontCheck (self.callHackage "doctest" "0.16.0.1" {});
            entropy                = self.callHackage "entropy" "0.4.1.3" {};
            foldl                  = self.callHackage "foldl" "1.4.5" {};
            haskell-src-exts       = self.callHackage "haskell-src-exts" "1.20.3" {};
            haskell-src-meta       = sel.haskell.lib.doJailbreak super.haskell-src-meta;
            hspec-core             = sel.haskell.lib.dontCheck super.hspec-core;
            lifted-async           = self.callHackage "lifted-async" "0.10.0.3" {};
            memory                 = self.callHackage "memory" "0.14.18" {};
            polyparse              = self.callHackage "polyparse" "1.12.1" {};
            quickcheck-instances   = self.callHackage "quickcheck-instances" "0.3.19" {};
            semigroupoids          = self.callHackage "semigroupoids" "5.3.1" {};
            system-fileio          = sel.haskell.lib.dontCheck super.system-fileio;
            tasty-expected-failure = sel.haskell.lib.doJailbreak super.tasty-expected-failure;
            th-expand-syns         = sel.haskell.lib.doJailbreak super.th-expand-syns;
            unliftio-core          = self.callHackage "unliftio-core" "0.1.2.0" {};
            unliftio               = self.callHackage "unliftio" "0.2.8.1" {};
            unordered-containers   = sel.haskell.lib.dontCheck super.unordered-containers;
          };
        };
      };
    };
  };
in
{ compiler ? "ghc862"
, nixpkgs ? import <nixpkgs> { overlays = [ overlay ]; }
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler nixpkgs packages pythonPackages rtsopts systemPackages; }
