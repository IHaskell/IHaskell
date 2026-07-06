sel: sup: {
  haskell = sup.haskell // {
    packages = sup.haskell.packages // {
      ghc910 = sup.haskell.packages.ghc910.override (old: {
        overrides = sel.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
          ghc-lib-parser = self.callHackage "ghc-lib-parser" "9.10.3.20250912" {};
          ghc-syntax-highlighter = self.callHackage "ghc-syntax-highlighter" "0.0.12.0" {};
        });
      });
    };
  };
}
