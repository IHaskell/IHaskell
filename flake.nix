{
  description = "A Haskell kernel for IPython.";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs?rev=a51aa6523bd8ee985bc70987909eff235900197a";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = inputs@{ self, hls, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux"] (system: let
      pkgs = import nixpkgs {
        inherit system;
      };
      compilerVersion = "8107";

      ihaskellEnv = import ./release.nix {
        compiler = "ghc${compilerVersion}";
        nixpkgs = pkgs;
        # just for the example
        packages = p: [ p.Frames ];
      };
    in {
      defaultPackage = ihaskellEnv;
    });
}
