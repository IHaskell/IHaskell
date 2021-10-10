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

      compilerVersion = "8107";

      pkgs = import nixpkgs {
        inherit system;
      };

      ihaskellEnv = import ./release.nix {
        compiler = "ghc${compilerVersion}";
        nixpkgs = pkgs;
      };

      hsPkgs = pkgs.haskell.packages."ghc${compilerVersion}";

      myModifier = drv:
        pkgs.haskell.lib.addBuildTools drv (with hsPkgs; [
          cabal-install
          self.inputs.hls.packages."${system}"."haskell-language-server-${compilerVersion}"
        ]);
    in {

      packages = {
        ihaskell = hsPkgs.developPackage {
          root =  pkgs.lib.cleanSource ./.;
          name = "ihaskell";
          returnShellEnv = false;
          modifier = pkgs.haskell.lib.dontCheck;
          overrides = ihaskellEnv.ihaskellOverlay;
          withHoogle = true;
        };

        ihaskellEnv = ihaskellEnv;
      };

      defaultPackage = self.packages.${system}.ihaskell;

      devShell = (myModifier self.packages.${system}.ihaskell).envFunc {withHoogle=true;};
    });
}
