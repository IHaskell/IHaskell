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
  haskellPackages = nixpkgs.haskell.packages."${compiler}".override (old: {
    overrides = nixpkgs.lib.composeExtensions (old.overrides or (_: _: {})) (self: super: {
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
      zeromq4-haskell   = self.callHackage "zeromq4-haskell" "0.8.0" {};

      haskell-src-exts  = self.haskell-src-exts_1_21_0;
      inline-r          = nixpkgs.haskell.lib.dontCheck super.inline-r;
      static-canvas     = nixpkgs.haskell.lib.doJailbreak super.static-canvas;
    } // displays self);
  });
  ihaskellEnv = haskellPackages.ghcWithPackages (self: [ self.ihaskell ] ++ packages self);
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ] ++ pythonPackages ps);

  ihaskellWrapperSh = nixpkgs.writeScriptBin "ihaskell-wrapper" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv jupyterlab ] ++ systemPackages nixpkgs)}''${PATH:+:}$PATH"
    exec ${ihaskellEnv}/bin/ihaskell "$@"
  '';

  ihaskellJupyterCmdSh = cmd: extraArgs: nixpkgs.writeScriptBin "ihaskell-${cmd}" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv jupyterlab ] ++ systemPackages nixpkgs)}''${PATH:+:}$PATH"
    ${ihaskellEnv}/bin/ihaskell install \
      -l $(${ihaskellEnv}/bin/ghc --print-libdir) \
      --use-rtsopts="${rtsopts}" \
      && ${jupyterlab}/bin/jupyter ${cmd} ${extraArgs} "$@"
  '';
in
nixpkgs.buildEnv {
  name = "ihaskell-with-packages";
  buildInputs = [ nixpkgs.makeWrapper ];
  paths = [ ihaskellEnv jupyterlab ];
  postBuild = ''
    ln -s ${ihaskellJupyterCmdSh "lab" ""}/bin/ihaskell-lab $out/bin/
    ln -s ${ihaskellJupyterCmdSh "notebook" ""}/bin/ihaskell-notebook $out/bin/
    ln -s ${ihaskellJupyterCmdSh "nbconvert" ""}/bin/ihaskell-nbconvert $out/bin/
    ln -s ${ihaskellJupyterCmdSh "console" "--kernel=haskell"}/bin/ihaskell-console $out/bin/
    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        wrapProgram $prg --set PYTHONPATH "$(echo ${jupyterlab}/lib/*/site-packages)"
      fi
    done
  '';

  passthru = {
    inherit ihaskellEnv;
    inherit jupyterlab;
    inherit ihaskellJupyterCmdSh;
    inherit ihaskellWrapperSh;
    ihaskellJsFile = ./. + "/html/kernel.js";
    ihaskellLogo64 = ./. + "/html/logo-64x64.svg";
  };
}
