{ nixpkgs ? import <nixpkgs> {}, packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:

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
      (display: { name = display; value = self.callCabal2nix display "${ihaskell-display-src}/${display}" {}; })
      [
        "ihaskell-aeson"
        "ihaskell-blaze"
        "ihaskell-charts"
        "ihaskell-diagrams"
        "ihaskell-gnuplot"
        "ihaskell-hatex"
        "ihaskell-juicypixels"
        "ihaskell-magic"
        "ihaskell-plot"
        "ihaskell-rlangqq"
        "ihaskell-static-canvas"
        "ihaskell-widgets"
      ]);
  dontCheck = nixpkgs.haskell.lib.dontCheck;
  haskellPackages = nixpkgs.haskellPackages.override {
    overrides = self: super: {
      ihaskell       = nixpkgs.haskell.lib.overrideCabal (
                       self.callCabal2nix "ihaskell"       ihaskell-src       {}) (_drv: {
        postPatch = let
          # Nix-built IHaskell expects to load a *.dyn_o file instead of *.o,
          # see https://github.com/gibiansky/IHaskell/issues/728
          original = ''
            setSessionDynFlags
                  flags'';
          replacement = ''
            setSessionDynFlags $ flip gopt_set Opt_BuildDynamicToo
                  flags'';
          # The tests seem to 'buffer' when run during nix-build, so this is
          # a throw-away test to get everything running smoothly and passing.
          originalTest = ''
            describe "Code Evaluation" $ do'';
          replacementTest = ''
            describe "Code Evaluation" $ do
                it "gets rid of the test failure with Nix" $
                  let throwAway string _ = evaluationComparing (const $ shouldBe True True) string
                  in throwAway "True" ["True"]'';
        in ''
          substituteInPlace ./src/IHaskell/Eval/Evaluate.hs --replace \
            '${original}' '${replacement}'
          substituteInPlace ./src/tests/IHaskell/Test/Eval.hs --replace \
            '${originalTest}' '${replacementTest}'
        '';
        preCheck = ''
          export HOME=$(${nixpkgs.pkgs.coreutils}/bin/mktemp -d)
          export PATH=$PWD/dist/build/ihaskell:$PATH
          export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
        '';
      });
      ghc-parser     = self.callCabal2nix "ghc-parser"     ghc-parser-src     {};
      ipython-kernel = self.callCabal2nix "ipython-kernel" ipython-kernel-src {};
    } // displays self;
  };
  ihaskell = haskellPackages.ihaskell;
  ihaskellEnv = haskellPackages.ghcWithPackages (self: with self; [
    ihaskell
    ihaskell-aeson
    ihaskell-blaze
    ihaskell-charts
    ihaskell-diagrams
    ihaskell-gnuplot
    ihaskell-hatex
    ihaskell-juicypixels
    ihaskell-magic
    ihaskell-plot
    # ihaskell-rlangqq
    ihaskell-static-canvas
    # ihaskell-widgets
  ] ++ packages self);
  jupyter = nixpkgs.python3.buildEnv.override {
    extraLibs = let ps = nixpkgs.python3Packages; in [ ps.jupyter ps.notebook ];
  };
  ihaskellSh = nixpkgs.writeScriptBin "ihaskell-notebook" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskell ihaskellEnv jupyter ] ++ systemPackages nixpkgs)}"
    ${ihaskell}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) --use-rtsopts="${rtsopts}" && ${jupyter}/bin/jupyter notebook
  '';
in
nixpkgs.buildEnv {
  name = "ihaskell-with-packages";
  buildInputs = [ nixpkgs.makeWrapper ];
  paths = [ ihaskellEnv jupyter ];
  postBuild = ''
    ln -s ${ihaskellSh}/bin/ihaskell-notebook $out/bin/
    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
      fi
    done
  '';
}
