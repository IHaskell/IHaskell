sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc98 = sup.haskell.packages.ghc98.override {
        overrides = self: super: {

          ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_11_0;

          # For display libs
          diagrams-lib = sup.haskell.lib.doJailbreak super.diagrams-lib;
          svg-builder = sup.haskell.lib.doJailbreak super.svg-builder;
          th-desugar = self.callHackage "th-desugar" "1.16" {};
          singletons-th = super.singletons-th_3_3;
          singletons-base = self.callHackage "singletons-base" "3.3" {};
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
