sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc810 = sup.haskell.packages.ghc810.override {
        overrides = self: super: {
          # Note that singletons-3.x broke the library into separate pieces like
          # singletons-th. singletons-3.x will build with GHC 8.10, but we don't have
          # the conditional compilation in the cabal file to handle this. The CPP
          # in IHaskell.Display.Widgets.Singletons needs updating too.
          singletons = self.callHackage "singletons" "2.7" {};
          th-desugar = self.callHackage "th-desugar" "1.11" {};
        };
      };
    };
  };
}
