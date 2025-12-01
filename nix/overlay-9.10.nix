_sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc910 = sup.haskell.packages.ghc910.override {
        overrides = self: super: {
        };
      };
    };
  };
}
