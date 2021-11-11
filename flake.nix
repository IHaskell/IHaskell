{
  description = "A Haskell kernel for IPython.";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, hls, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let

      pkgs = import nixpkgs {
        inherit system;
      };

      ihaskellEnv = compilerVersion:
        import ./release.nix {
          compiler = "ghc${compilerVersion}";
          nixpkgs = pkgs;
        };

      ghc884  = pkgs.haskell.packages.ghc884;
      ghc8107  = pkgs.haskell.packages.ghc8107;

      mkDevShell = hsPkgs:
        let
          myIHaskell = (mkPackage hsPkgs);
          compilerVersion = pkgs.lib.replaceStrings [ "." ] [ "" ] hsPkgs.ghc.version;

          myModifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with hsPkgs; [
              cabal-install
              self.inputs.hls.packages.${system}."haskell-language-server-${compilerVersion}"
            ]);
        in (myModifier myIHaskell).envFunc {withHoogle=true;};


      mkPackage = hsPkgs:
        let
          compilerVersion = pkgs.lib.replaceStrings [ "." ] [ "" ] hsPkgs.ghc.version;
        in
        hsPkgs.developPackage {
          root =  pkgs.lib.cleanSource ./.;
          name = "ihaskell";
          returnShellEnv = false;
          modifier = pkgs.haskell.lib.dontCheck;
          overrides = (ihaskellEnv compilerVersion).ihaskellOverlay ;
          withHoogle = true;
        };

    in {

      packages = {
        ihaskell = mkDevShell ghc8107;
        ihaskell8107 = mkDevShell ghc8107;
        ihaskell884 = mkDevShell ghc884;
        ihaskellEnv = ihaskellEnv ghc8107;
      };

      defaultPackage = self.packages.${system}.ihaskell;
    });
}
