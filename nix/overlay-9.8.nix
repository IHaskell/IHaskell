_sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc98 = sup.haskell.packages.ghc98.override {
        overrides = self: super: {
          # ghc-syntax-highlighter = (self.callHackage "ghc-syntax-highlighter" "0.0.11.0" {}).overrideScope(self: _super: {
          #   ghc-lib-parser = self.ghc-lib-parser_9_8_3_20241022;
          # });

          # For display libs
          singletons-base = self.callHackage "singletons-base" "3.3" {};
          singletons-th = sup.haskell.lib.doJailbreak (self.callHackage "singletons-th" "3.3" {});
          th-desugar = self.callHackage "th-desugar" "1.16" {};
          plot = self.callHackage "plot" "0.2.3.12" {};
        };
      };
    };
  };
}
