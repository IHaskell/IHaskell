_sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc912 = sup.haskell.packages.ghc912.override {
        overrides = self: super: {

          ghc-syntax-highlighter = self.ghc-syntax-highlighter_0_0_13_0;

          # entropy = self.callHackageDirect {
          #   pkg = "entropy";
          #   ver = "0.4.1.11";
          #   sha256 = "sha256-gtByk35iSFLF7Y06rgJzjHBQUKBtVp2A/PdXb7OLJBo=";
          # } {};

          generically = sup.haskell.lib.doJailbreak super.generically;

          # hlint = self.callCabal2nix "hlint" (builtins.fetchTarball {
          #   url = "https://github.com/ndmitchell/hlint/tarball/7dfba720eaf6fa9bd0b23ae269334559aa722847";
          #   sha256 = "06sqja2n9glj8f58hkcpbkjf1h70x22jv74h9pzdlsp459sq28cy";
          # }) {};

          indexed-traversable = sup.haskell.lib.doJailbreak super.indexed-traversable;
          these = sup.haskell.lib.doJailbreak super.these;
          code-page = sup.haskell.lib.doJailbreak (sup.haskell.lib.dontCheck super.code-page);
          primitive = sup.haskell.lib.doJailbreak super.primitive;
          call-stack = sup.haskell.lib.dontCheck super.call-stack;
          doctest = sup.haskell.lib.doJailbreak (sup.haskell.lib.dontCheck super.doctest);
          hashable = sup.haskell.lib.doJailbreak (super.hashable.overrideScope(_sel: _sup: {
            os-string = null;
          }));
          ChasingBottoms = sup.haskell.lib.doJailbreak super.ChasingBottoms;
          # th-abstraction = super.th-abstraction_0_7_1_0;
          nothunks = sup.haskell.lib.doJailbreak super.nothunks;
          uuid = sup.haskell.lib.doJailbreak super.uuid;
          time-compat = sup.haskell.lib.doJailbreak super.time-compat;
          scientific = sup.haskell.lib.doJailbreak super.scientific;
          quickcheck-instances = sup.haskell.lib.doJailbreak super.quickcheck-instances;
          # aeson = sup.haskell.lib.doJailbreak super.aeson_2_2_3_0;
          integer-conversion = sup.haskell.lib.doJailbreak super.integer-conversion;
          hourglass = sup.haskell.lib.dontCheck super.hourglass;
          lifted-base = sup.haskell.lib.dontCheck (sup.haskell.lib.doJailbreak super.lifted-base);
          zlib = sup.haskell.lib.doJailbreak super.zlib;
          cryptonite = sup.haskell.lib.dontCheck super.cryptonite;
        };
      };
    };
  };
}
