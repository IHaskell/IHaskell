sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc92 = sup.haskell.packages.ghc92.override {
        overrides = self: super: {
          singletons-base = self.callHackage "singletons-base" "3.1" {};
          singletons-th = self.callHackage "singletons-th" "3.1" {};
          singletons = self.callHackage "singletons" "3.0.1" {};
          th-desugar = self.callHackage "th-desugar" "1.13.1" {};
          Chart-cairo = self.callHackage "Chart-cairo" "1.9.3" {};
        };
      };
    };
  };
}
