sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc910 = sup.haskell.packages.ghc910.override {
        overrides = self: super: {

          ghc-syntax-highlighter = sup.haskell.lib.doJailbreak self.ghc-syntax-highlighter_0_0_12_0;

          assoc = sup.haskell.lib.doJailbreak super.assoc;
          code-page = sup.haskell.lib.doJailbreak (sup.haskell.lib.dontCheck super.code-page);
          generically = sup.haskell.lib.doJailbreak super.generically;
          primitive = sup.haskell.lib.doJailbreak super.primitive;
          call-stack = sup.haskell.lib.dontCheck super.call-stack;
          doctest = sup.haskell.lib.doJailbreak (sup.haskell.lib.dontCheck super.doctest);
          hashable = sup.haskell.lib.doJailbreak (super.hashable.overrideScope(sel: sup: {
            os-string = null;
          }));
          unliftio-core = sup.haskell.lib.doJailbreak super.unliftio-core;
          ChasingBottoms = sup.haskell.lib.doJailbreak super.ChasingBottoms;
          th-abstraction = super.th-abstraction_0_7_0_0;
          indexed-traversable = sup.haskell.lib.doJailbreak super.indexed-traversable;
          nothunks = sup.haskell.lib.doJailbreak super.nothunks;
          uuid-types = sup.haskell.lib.doJailbreak super.uuid-types;
          these = sup.haskell.lib.doJailbreak super.these;
          time-compat = sup.haskell.lib.doJailbreak super.time-compat;
          scientific = sup.haskell.lib.doJailbreak super.scientific;
          quickcheck-instances = sup.haskell.lib.doJailbreak super.quickcheck-instances;
          indexed-traversable-instances = sup.haskell.lib.doJailbreak super.indexed-traversable-instances;
          semialign = sup.haskell.lib.doJailbreak super.semialign;
          aeson = sup.haskell.lib.doJailbreak super.aeson_2_2_3_0;
          integer-conversion = sup.haskell.lib.doJailbreak super.integer-conversion;
          boring = sup.haskell.lib.doJailbreak super.boring;
          hourglass = sup.haskell.lib.dontCheck super.hourglass;
          lifted-base = sup.haskell.lib.dontCheck (sup.haskell.lib.doJailbreak super.lifted-base);
          zlib = sup.haskell.lib.doJailbreak super.zlib;
        };
      };
    };
  };
}
