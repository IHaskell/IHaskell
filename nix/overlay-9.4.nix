sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc94 = sup.haskell.packages.ghc94.override {
        overrides = self: super: {
          ghc-syntax-highlighter = super.ghc-syntax-highlighter_0_0_10_0;
          ghc-lib-parser = super.ghc-lib-parser_9_6_3_20231014;
          ghc-lib-parser-ex = super.ghc-lib-parser-ex_9_6_0_2;
        };
      };
    };
  };
}
