_sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc914 = sup.haskell.packages.ghc914.override {
        overrides = self: super: {
          ghc-lib-parser = self.ghc-lib-parser_9_14_1_20251220;
          ghc-lib-parser-ex = self.ghc-lib-parser-ex_9_14_2_0;
          ghc-syntax-highlighter = self.callHackage "ghc-syntax-highlighter" "0.0.14.0" {};

          boring = sup.haskell.lib.doJailbreak super.boring;
          lifted-async = sup.haskell.lib.doJailbreak super.lifted-async;
          uuid = sup.haskell.lib.doJailbreak super.uuid;

          hlint = self.callCabal2nix "hlint" (builtins.fetchTarball {
            # https://github.com/ndmitchell/hlint/pull/1673
            url = "https://github.com/ndmitchell/hlint/tarball/ba7b2d0a0faabc5fcf2e57ffdeffd793f2fdcc3d";
            sha256 = "sha256:0j6bng3ij9zaj1gkxdrfmq2vb2z1czii25azz49ki6vn6aqsxzbq";
          }) {};
        };
      };
    };
  };
}
