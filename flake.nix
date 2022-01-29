{
  description = "A Haskell kernel for IPython.";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    hls.url = "github:haskell/haskell-language-server";

    flake-compat = {
      url = "github:teto/flake-compat/support-packages";
      flake = false;
    };
  };

  outputs = { self, hls, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachSystem ["x86_64-linux" "x86_64-darwin"] (system: let

      pkgs = import nixpkgs {
        inherit system;
        overlays = [ self.overlay ];
      };

      compilerVersionFromHsPkgs = hsPkgs:
        pkgs.lib.replaceStrings [ "." ] [ "" ] hsPkgs.ghc.version;

      mkEnv = hsPkgs: let
        compilerVersion = compilerVersionFromHsPkgs hsPkgs;
      in
        import (./. + "/release.nix") {
          compiler = "ghc${compilerVersion}";
          nixpkgs = pkgs;
        };

      mkExe = hsPkgs: (mkEnv hsPkgs).ihaskellExe;

      ghcDefault = ghc8107;
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
              # pythonDevEnv
              self.inputs.hls.packages.${system}."haskell-language-server-${compilerVersion}"
            ]);
        in (myModifier myIHaskell).envFunc {withHoogle=false;};


      mkPackage = hsPkgs:
        let
          compilerVersion = pkgs.lib.replaceStrings [ "." ] [ "" ] hsPkgs.ghc.version;
        in
        hsPkgs.developPackage {
          root =  pkgs.lib.cleanSource ./.;
          name = "ihaskell";
          returnShellEnv = false;
          modifier = pkgs.haskell.lib.dontCheck;
          overrides = (mkEnv hsPkgs).ihaskellOverlay;
          withHoogle = false;
        };

    in {

      packages = {
        ihaskell-dev = mkDevShell ghcDefault;
        ihaskell-921-dev = mkDevShell ghc921;
        ihaskell-8107-dev = mkDevShell ghc8107;

        ihaskell = mkExe ghcDefault;
        ihaskell-8107 = mkExe ghc8107;
        ihaskell-921 = mkExe ghc921;

        # I actually wish those would disappear ? let jupyterWith or use deal with it
        ihaskell-env = mkEnv ghcDefault;
        ihaskell-8107-env = mkEnv ghc8107;
        ihaskell-921-env = mkEnv ghc921;
      };

      defaultPackage = self.packages.${system}.ihaskell;

      devShells = {
        ihaskell-dev = mkDevShell ghcDefault;
        ihaskell-8107 = mkDevShell ghc8107;
        ihaskell-921 = mkDevShell ghc921;
      };
    }) // {

      overlay = final: prev: {
        python3 = prev.python3.override {
          packageOverrides = pfinal: pprev: {
            openapi-core = pprev.openapi-core.overridePythonAttrs(oa: {

              doCheck = false;
              doInstallCheck = false;
            });
          };
        };

        ihaskellGhc921 = prev.haskell.packages.ghc921.extend (final: prev: let 
          displays = final: builtins.listToAttrs (
            map
              (display: { name = "ihaskell-${display}"; value = final.callCabal2nix display "${ihaskell-src}/ihaskell-display/ihaskell-${display}" {}; })
              [ "aeson" "blaze" "charts" "diagrams" "gnuplot" "graphviz" "hatex" "juicypixels" "magic" "plot" "rlangqq" "static-canvas" "widgets" ]);
          ihaskell-src = final.nix-gitignore.gitignoreSource
            [ "**/*.ipynb" "**/*.nix" "**/*.yaml" "**/*.yml" "**/\.*" "/Dockerfile" "/README.md" "/cabal.project" "/images" "/notebooks" "/requirements.txt" ]
            ./.;
          in {
          ihaskell          = (final.haskell.lib.overrideCabal (
                              final.callCabal2nix "ihaskell" ihaskell-src {}) (_drv: {
            preCheck = ''
              export HOME=$TMPDIR/home
              export PATH=$PWD/dist/build/ihaskell:$PATH
              export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
            '';
            configureFlags = (_drv.configureFlags or []) ++ [ "-f" "-use-hlint" ];
          })).overrideScope (final: prev: {
            hlint = null;
          });
          ghc-parser        = final.callCabal2nix "ghc-parser" ./ghc-parser {};
          ipython-kernel    = final.callCabal2nix "ipython-kernel" ./ipython-kernel {};

          aeson = prev.aeson_2_0_3_0;
        } // displays final);

        # ihaskellGhc902 = final.haskell.packages.ghc902.extend({
        # });

      };
    };
}
