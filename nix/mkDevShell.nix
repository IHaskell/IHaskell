{ pkgs

, hls
, system
, version
, haskellPackages
, ihaskellOverlay
}:

let
  compilerVersion = builtins.substring 3 100 version;

  devIHaskell = haskellPackages.developPackage {
    root =  pkgs.lib.cleanSource ../.;
    name = "ihaskell";
    returnShellEnv = false;
    modifier = pkgs.haskell.lib.dontCheck;
    overrides = ihaskellOverlay;
    withHoogle = true;
  };

  devModifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with haskellPackages; [
      cabal-install
      (pkgs.python3.withPackages (p: [p.jupyterlab]))

      # Note: HLS support currently doesn't work, because the HLS project has removed
      # HLS binaries from their flake; see
      # https://github.com/haskell/haskell-language-server/pull/3804
      # hls.packages.${system}."haskell-language-server-${compilerVersion}"

      pkgs.cairo # for the ihaskell-charts HLS dev environment
      pkgs.pango # for the ihaskell-diagrams HLS dev environment
      pkgs.lapack # for the ihaskell-plot HLS dev environment
      pkgs.blas # for the ihaskell-plot HLS dev environment
    ]);

in

(devModifier devIHaskell).envFunc {withHoogle=true;}
