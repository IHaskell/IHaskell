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
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin" "aarch64-darwin"] (system: let

      pkgs = import nixpkgs {
        inherit system;
      };

      compilerVersionFromHsPkgs = hsPkgs:
        pkgs.lib.replaceStrings [ "." ] [ "" ] hsPkgs.ghc.version;

      mkEnv = hsPkgs: displayPkgs:
        import ./release.nix {
          compiler = "ghc${compilerVersionFromHsPkgs hsPkgs}";
          nixpkgs = pkgs;
          packages = displayPkgs;
          systemPackages = p: with p; [
            gnuplot # for the ihaskell-gnuplot runtime
          ];
        };

      mkExe = hsPkgs: (mkEnv hsPkgs (_:[])).ihaskellExe;

      ghcDefault = ghc8107;
      ghc884  = pkgs.haskell.packages.ghc884;
      ghc8107  = pkgs.haskell.packages.ghc8107;
      ghc921  = pkgs.haskell.packages.ghc921;

      pythonDevEnv = pkgs.python3.withPackages (p: [p.jupyterlab]);

      mkDevShell = hsPkgs:
        let
          myIHaskell = (mkPackage hsPkgs);
          compilerVersion = compilerVersionFromHsPkgs hsPkgs;

          myModifier = drv:
            pkgs.haskell.lib.addBuildTools drv (with hsPkgs; [
              cabal-install
              pythonDevEnv
              self.inputs.hls.packages.${system}."haskell-language-server-${compilerVersion}"
              pkgs.cairo # for the ihaskell-charts HLS dev environment
              pkgs.pango # for the ihaskell-diagrams HLS dev environment
              pkgs.lapack # for the ihaskell-plot HLS dev environment
              pkgs.blas # for the ihaskell-plot HLS dev environment
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
          overrides = (mkEnv hsPkgs (_:[])).ihaskellOverlay ;
          withHoogle = true;
        };

    in {

      packages = {
        # Development environment
        ihaskell-dev = mkDevShell ghcDefault;
        ihaskell-dev-921 = mkDevShell ghc921;
        ihaskell-dev-8107 = mkDevShell ghc8107;
        ihaskell-dev-884 = mkDevShell ghc884;

        # IHaskell kernel
        ihaskell = mkExe ghcDefault;
        ihaskell-8107 = mkExe ghc8107;
        ihaskell-921 = mkExe ghc921;

        # Full Jupyter environment
        # I actually wish those would disappear ? let jupyterWith or use deal with it
        ihaskell-env = mkEnv ghcDefault (_:[]);
        ihaskell-env-8107 = mkEnv ghc8107 (_:[]);

        # Full Jupyter environment with all Display modules (build is not incremental)
        #
        #     result/bin/jupyter-lab
        #
        ihaskell-env-display = mkEnv ghcDefault (p: with p; [
            ihaskell-aeson
            ihaskell-blaze
            ihaskell-charts
            ihaskell-diagrams
            ihaskell-gnuplot
            ihaskell-graphviz
            ihaskell-hatex
            ihaskell-juicypixels
            ihaskell-magic
            ihaskell-plot
            ihaskell-widgets
            ]);
      };

      defaultPackage = self.packages.${system}.ihaskell;

      devShell = self.packages.${system}.ihaskell-dev;
    });
}
