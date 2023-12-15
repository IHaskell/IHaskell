{ callPackage
, haskell
, lib

, enableHlint
}:

self: super:

let
  ihaskell-src = callPackage ./ihaskell-src.nix {};

  displays = let
    mkDisplay = display: {
      name = display;
      value = self.callCabal2nix display "${ihaskell-src}/ihaskell-display/${display}" {};
    };
  in
    builtins.listToAttrs (map mkDisplay (import ./displays.nix));

in

{
  ihaskell = let
    baseIhaskell = haskell.lib.overrideCabal (self.callCabal2nix "ihaskell" ihaskell-src {}) (_drv: {
      preCheck = ''
        export HOME=$TMPDIR/home
        export PATH=$PWD/dist/build/ihaskell:$PATH
        export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
      '';
      configureFlags = (_drv.configureFlags or []) ++ (lib.optionals (!enableHlint) [ "-f" "-use-hlint" ]);
    });
  in
    if enableHlint
    then baseIhaskell
    else baseIhaskell.overrideScope (self: super: { hlint = null; });

  ghc-parser     = self.callCabal2nix "ghc-parser" (builtins.path { path = ../ghc-parser; name = "ghc-parser-src"; }) {};
  ipython-kernel = self.callCabal2nix "ipython-kernel" (builtins.path { path = ../ipython-kernel; name = "ipython-kernel-src"; }) {};
} // displays
