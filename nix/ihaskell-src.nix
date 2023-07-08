{ nix-filter
}:

nix-filter {
  root = ../.;
  include = [
    "LICENSE"
    "Setup.hs"
    "ghc-parser"
    "html"
    "ihaskell-display"
    "ihaskell.cabal"
    "ipython-kernel"
    "jupyterlab-ihaskell"
    "main"
    "src"
    "test"
  ];
}
