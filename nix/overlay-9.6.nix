sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc96 = sup.haskell.packages.ghc96.override {
        overrides = self: super: {

          # https://github.com/amcphail/plot/pull/23
          plot = super.callCabal2nix "plot" (sup.fetchFromGitHub {
            owner = "codedownio";
            repo = "haskell-plot";
            rev = "dfa26022b5815bcd6a5dd6c818fcd2c4d25c6d44";
            sha256 = "1snk70l7q98cqflgaqf6l75g4hpcnf284flm9rsmk8kkzd5nnh5k";
          }) {};

          singletons-base = sup.haskell.lib.dontCheck (self.callHackage "singletons-base" "3.2" {});
          singletons-th = self.callHackage "singletons-th" "3.2" {};
          th-desugar = self.callHackage "th-desugar" "1.15" {};
        };
      };
    };
  };
}
