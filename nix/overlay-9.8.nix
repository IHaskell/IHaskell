sel: sup: {
  all-cabal-hashes = sup.fetchurl {
    url = "https://github.com/commercialhaskell/all-cabal-hashes/tarball/d77837f979c4b15fe0eb25cdf8a0463773434c9d";
    sha256 = "0yl1ahbpn5w6n4xcgc23zcfr21f79dz7vz1xjlkzsvxddc2v8nm0";
  };
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc98 = sup.haskell.packages.ghc98.override {
        overrides = self: super: {
          aeson = self.callHackage "aeson" "2.2.1.0" {};
          aeson-pretty = super.aeson-pretty_0_8_10;
          attoparsec-aeson = super.attoparsec-aeson_2_2_0_1;

          alex = sup.haskell.lib.dontCheck super.alex;
          bifunctors = self.callHackage "bifunctors" "5.6.1" {};
          doctest = super.doctest_0_22_2;
          ghc-lib-parser = self.callHackage "ghc-lib-parser" "9.8.1.20231121" {};
          ghc-lib-parser-ex = super.ghc-lib-parser-ex_9_8_0_0;
          # ghc-syntax-highlighter = self.callCabal2nix "ghc-syntax-highlighter" (sup.fetchFromGitHub {
          #   owner = "mrkkrp";
          #   repo = "ghc-syntax-highlighter";
          #   rev = "336df42e9185c2e2a91fa71eaa18b51f0f5437de";
          #   sha256 = "0nnpyq3z6c90z3j8xzw2gcj13nhihc7xa0sb4xbbdpxfnidplp9q";
          # }) {};
          # # ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_11_0;
          here = sup.haskell.lib.doJailbreak (self.callHackage "here" "1.2.14" {});
          hourglass = sup.haskell.lib.dontCheck super.hourglass;

          hspec = self.callHackage "hspec" "2.11.6" {};
          hspec-core = sup.haskell.lib.dontCheck (self.callHackage "hspec-core" "2.11.6" {});
          hspec-discover = self.callHackage "hspec-discover" "2.11.6" {};
          hspec-meta = self.callHackage "hspec-meta" "2.11.6" {};

          hspec-expectations = self.callHackage "hspec-expectations" "0.8.4" {};

          lifted-base = sup.haskell.lib.dontCheck super.lifted-base;
          semigroupoids = self.callHackage "semigroupoids" "6.0.0.1" {};
          shelly = sup.haskell.lib.doJailbreak super.shelly;
          th-abstraction = self.callHackage "th-abstraction" "0.6.0.0" {};
          th-lift = self.callHackage "th-lift" "0.8.4" {};
          unliftio-core = sup.haskell.lib.doJailbreak super.unliftio-core;

          tagged = super.tagged_0_8_8;

          # For display libs
          statestack = sup.haskell.lib.doJailbreak super.statestack;
          diagrams-core = self.callHackage "diagrams-core" "1.5.1.1" {};
          diagrams-lib = sup.haskell.lib.doJailbreak super.diagrams-lib;
          svg-builder = sup.haskell.lib.doJailbreak super.svg-builder;
          th-desugar = self.callHackage "th-desugar" "1.16" {};
          free = super.free_5_2;
          turtle = super.turtle_1_6_2;
          singletons-th = super.singletons-th_3_3;
          singletons-base = super.singletons-base_3_3;
          newtype-generics = sup.haskell.lib.doJailbreak super.newtype-generics;

          # These ones have a non-working version of gtk2hs-buildtools added via addBuildTool
          # in Nixpkgs. Override their cabal files to remove it.
          cairo = sup.haskell.lib.overrideCabal (sup.haskell.lib.doJailbreak super.cairo) (_: { buildTools = []; });
          pango = sup.haskell.lib.overrideCabal (sup.haskell.lib.doJailbreak super.pango) (_: { buildTools = []; });
          glib = sup.haskell.lib.overrideCabal (sup.haskell.lib.doJailbreak super.glib) (_: { buildTools = []; });

          # https://github.com/amcphail/plot/pull/23
          plot = super.callCabal2nix "plot" (sup.fetchFromGitHub {
            owner = "codedownio";
            repo = "haskell-plot";
            rev = "dfa26022b5815bcd6a5dd6c818fcd2c4d25c6d44";
            sha256 = "1snk70l7q98cqflgaqf6l75g4hpcnf284flm9rsmk8kkzd5nnh5k";
          }) {};

          force-layout = sup.haskell.lib.doJailbreak super.force-layout;
          active = sup.haskell.lib.doJailbreak super.active;
          diagrams-svg = sup.haskell.lib.doJailbreak super.diagrams-svg;
          diagrams-cairo = sup.haskell.lib.doJailbreak super.diagrams-cairo;
          diagrams-contrib = sup.haskell.lib.doJailbreak super.diagrams-contrib;
        };
      };
    };
  };
}
