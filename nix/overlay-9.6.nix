_sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc96 = sup.haskell.packages.ghc96.override {
        overrides = self: _super: {

          plot = self.callHackage "plot" "0.2.3.12" {};

          singletons-base = sup.haskell.lib.dontCheck (self.callHackage "singletons-base" "3.2" {});
          singletons-th = self.callHackage "singletons-th" "3.2" {};
          th-desugar = self.callHackage "th-desugar" "1.15" {};
        };
      };
    };
  };
}
