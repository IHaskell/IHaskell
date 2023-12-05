sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc98 = sup.haskell.packages.ghc98.override {
        overrides = self: super: {
          aeson = self.callHackage "aeson" "2.2.1.0" {};
          alex = sup.haskell.lib.dontCheck super.alex;
          bifunctors = self.callHackage "bifunctors" "5.6.1" {};
          doctest = self.callHackage "doctest" "0.22.2" {};
          ghc-lib-parser = sup.haskell.lib.overrideCabal (self.callHackage "ghc-lib-parser" "9.8.1.20231009" {}) (_drv: {
            postPatch = let
              original-a = "cabal-version: 2.0";
              replacement-a = "cabal-version: 3.0";
              original-b = "license: BSD3";
              replacement-b = "license: BSD-3-Clause";
              original-c =
                "c-sources:\r\n        libraries/ghc-heap/cbits/HeapPrim.cmm";
              replacement-c =
                "cmm-sources:\r\n        libraries/ghc-heap/cbits/HeapPrim.cmm";
            in ''
                substituteInPlace ./ghc-lib-parser.cabal --replace \
                  '${original-a}' '${replacement-a}'
                substituteInPlace ./ghc-lib-parser.cabal --replace \
                  '${original-b}' '${replacement-b}'
                substituteInPlace ./ghc-lib-parser.cabal --replace \
                  '${original-c}' '${replacement-c}'
              '';
          });
          ghc-lib-parser-ex = super.ghc-lib-parser-ex_9_8_0_0;
          ghc-syntax-highlighter = self.callCabal2nix "ghc-syntax-highlighter" (sup.fetchFromGitHub {
            owner = "mrkkrp";
            repo = "ghc-syntax-highlighter";
            rev = "336df42e9185c2e2a91fa71eaa18b51f0f5437de";
            sha256 = "0nnpyq3z6c90z3j8xzw2gcj13nhihc7xa0sb4xbbdpxfnidplp9q";
          }) {};
          haskell-src-meta = sup.haskell.lib.appendPatch (sup.haskell.lib.doJailbreak super.haskell-src-meta) (sup.fetchpatch {
            url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/master/patches/haskell-src-meta-0.8.12.patch";
            sha256 = "0wn0lnq522bhc65sw02jm448mg5ijdxwgs7gzp9xp6z8ir1a0bzr";
          });
          here = sup.haskell.lib.doJailbreak (self.callHackage "here" "1.2.14" {});
          hourglass = sup.haskell.lib.dontCheck super.hourglass;
          hspec = self.callHackage "hspec" "2.11.6" {};
          hspec-core = sup.haskell.lib.dontCheck (self.callHackage "hspec-core" "2.11.6" {});
          hspec-discover = self.callHackage "hspec-discover" "2.11.6" {};
          hspec-expectations = self.callHackage "hspec-expectations" "0.8.4" {};
          hspec-meta = self.callHackage "hspec-meta" "2.11.6" {};
          lifted-base = sup.haskell.lib.dontCheck super.lifted-base;
          semigroupoids = self.callHackage "semigroupoids" "6.0.0.1" {};
          shelly = sup.haskell.lib.doJailbreak super.shelly;
          tagged = self.callHackage "tagged" "0.8.8" {};
          th-abstraction = self.callHackage "th-abstraction" "0.6.0.0" {};
          th-lift = self.callHackage "th-lift" "0.8.4" {};
          unliftio-core = sup.haskell.lib.doJailbreak super.unliftio-core;

          # For display libs (in progress)
          statestack = sup.haskell.lib.doJailbreak super.statestack;
          # TODO: wait for nixpkgs master to update Hackage pin to have this diagrams-core version
          diagrams-core = self.callHackage "diagrams-core" "1.5.1.1" {};
          diagrams-lib = sup.haskell.lib.doJailbreak super.diagrams-lib;
        };
      };
    };
  };
}
