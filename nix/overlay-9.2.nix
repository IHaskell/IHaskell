sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc92 = sup.haskell.packages.ghc92.override {
        overrides = self: super: {
          Chart-cairo = sup.haskell.lib.doJailbreak super.Chart-cairo;
          # spurious test failures
          singletons-base = sup.haskell.lib.dontCheck (sup.haskell.lib.doJailbreak super.singletons-base);
          singletons-th = sup.haskell.lib.doJailbreak super.singletons-th;
        };
      };
    };
  };
}
