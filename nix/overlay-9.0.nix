sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc90 = sup.haskell.packages.ghc90.override {
        overrides = self: super: {
          singletons-base = self.callHackage "singletons-base" "3.0" {};
          singletons-th = self.callHackage "singletons-th" "3.0" {};
          th-desugar = self.callHackage "th-desugar" "1.12" {};
        };
      };
    };
  };
}
