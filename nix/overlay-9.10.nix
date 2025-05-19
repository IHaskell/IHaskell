_sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc910 = sup.haskell.packages.ghc910.override {
        overrides = self: super: {

          ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_13_0;
          hlint = self.hlint_3_10;
          ghc-lib-parser = self.ghc-lib-parser_9_12_2_20250320;
          ghc-lib-parser-ex = self.ghc-lib-parser-ex_9_12_0_0;

          code-page = sup.haskell.lib.doJailbreak (sup.haskell.lib.dontCheck super.code-page);
          primitive = sup.haskell.lib.doJailbreak super.primitive;
          call-stack = sup.haskell.lib.dontCheck super.call-stack;
          doctest = sup.haskell.lib.doJailbreak (sup.haskell.lib.dontCheck super.doctest);
          ChasingBottoms = sup.haskell.lib.doJailbreak super.ChasingBottoms;
          nothunks = sup.haskell.lib.doJailbreak super.nothunks;
          uuid = sup.haskell.lib.doJailbreak super.uuid;
          time-compat = sup.haskell.lib.doJailbreak super.time-compat;
          scientific = sup.haskell.lib.doJailbreak super.scientific;
          quickcheck-instances = sup.haskell.lib.doJailbreak super.quickcheck-instances;
          integer-conversion = sup.haskell.lib.doJailbreak super.integer-conversion;
          hourglass = sup.haskell.lib.dontCheck super.hourglass;
          lifted-base = sup.haskell.lib.dontCheck (sup.haskell.lib.doJailbreak super.lifted-base);
          zlib = sup.haskell.lib.doJailbreak super.zlib;
        };
      };
    };
  };
}
