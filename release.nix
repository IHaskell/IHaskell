{ compiler ? "ghc802"
, nixpkgs ? import <nixpkgs> {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, systemPackages ? (_: [])
}:

let
  inherit (builtins) any elem filterSource listToAttrs;
  lib = nixpkgs.lib;
  cleanSource = name: type: let
    baseName = baseNameOf (toString name);
  in lib.cleanSourceFilter name type && !(
    (type == "directory" && (elem baseName [ ".stack-work" "dist"])) ||
    any (lib.flip lib.hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ]
  );
  ihaskellSourceFilter = src: name: type: let
    relPath = lib.removePrefix (toString src + "/") (toString name);
  in cleanSource name type && ( any (lib.flip lib.hasPrefix relPath) [
    "src" "main" "html" "Setup.hs" "ihaskell.cabal" "LICENSE"
  ]);
  ihaskell-src         = filterSource (ihaskellSourceFilter ./.) ./.;
  ipython-kernel-src   = filterSource cleanSource ./ipython-kernel;
  ghc-parser-src       = filterSource cleanSource ./ghc-parser;
  ihaskell-display-src = filterSource cleanSource ./ihaskell-display;
  displays = self: listToAttrs (
    map
      (display: { name = "ihaskell-${display}"; value = self.callCabal2nix display "${ihaskell-display-src}/ihaskell-${display}" {}; })
      [ "aeson" "blaze" "charts" "diagrams" "gnuplot" "graphviz" "hatex" "juicypixels" "magic" "plot" "rlangqq" "static-canvas" "widgets" ]);
  haskellPackages = nixpkgs.haskell.packages."${compiler}".extend (self: super: {
    ihaskell          = nixpkgs.haskell.lib.overrideCabal (
                        self.callCabal2nix "ihaskell" ihaskell-src {}) (_drv: {
      preCheck = ''
        export HOME=$(${nixpkgs.pkgs.coreutils}/bin/mktemp -d)
        export PATH=$PWD/dist/build/ihaskell:$PATH
        export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
      '';
      configureFlags = (_drv.configureFlags or []) ++ [
        # otherwise the tests are agonisingly slow and the kernel times out
        "--enable-executable-dynamic"
      ];
      doHaddock = false;
    });
    ghc-parser        = self.callCabal2nix "ghc-parser" ghc-parser-src {};
    ipython-kernel    = self.callCabal2nix "ipython-kernel" ipython-kernel-src {};

    haskell-src-exts  = self.haskell-src-exts_1_20_2;

    static-canvas     = nixpkgs.haskell.lib.doJailbreak super.static-canvas;
  } // displays self);
  ihaskellEnv = haskellPackages.ghcWithPackages (self: [ self.ihaskell ] ++ packages self);
  notebook = nixpkgs.python3.withPackages (ps: [ ps.notebook ] ++ pythonPackages ps);
  ihaskellSh = cmd: extraArgs: nixpkgs.writeScriptBin "ihaskell-${cmd}" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv notebook ] ++ systemPackages nixpkgs)}:$PATH"
    ${ihaskellEnv}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) --use-rtsopts="${rtsopts}" && ${notebook}/bin/jupyter ${cmd} ${extraArgs} "$@"
  '';
in
nixpkgs.buildEnv {
  name = "ihaskell-with-packages";
  buildInputs = [ nixpkgs.makeWrapper ];
  paths = [ ihaskellEnv notebook ];
  postBuild = ''
    ln -s ${ihaskellSh "notebook" ""}/bin/ihaskell-notebook $out/bin/
    ln -s ${ihaskellSh "nbconvert" ""}/bin/ihaskell-nbconvert $out/bin/
    ln -s ${ihaskellSh "console" "--kernel=haskell"}/bin/ihaskell-console $out/bin/
    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        wrapProgram $prg --set PYTHONPATH "$(echo ${notebook}/lib/*/site-packages)"
      fi
    done
  '';
}
