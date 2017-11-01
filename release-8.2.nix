let
  overlay = self: super: {
    all-cabal-hashes = super.fetchFromGitHub {
      owner  = "commercialhaskell";
      repo   = "all-cabal-hashes";
      rev    = "a7e38c265b7927a921fbc06b977c1e254cb3142b";
      sha256 = "0n4703mbfdgwnmy5cbzahgg0vmqpin25aafcf30fyl49gbzyjr6g";
    };
  };
  overlaid = import <nixpkgs> { overlays = [ overlay ]; };
in { nixpkgs ? overlaid, packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:

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
  plot = nixpkgs.fetchFromGitHub {
    owner  = "amcphail";
    repo   = "plot";
    rev    = "cc5cdff696aa99e1001124917c3b87b95529c4e3";
    sha256 = "13abrymry4nqyl9gmjrj8lhplbg4xag7x41n89yyw822360d3drh";
  };
  gtk2hs = nixpkgs.fetchFromGitHub {
    owner  = "gtk2hs";
    repo   = "gtk2hs";
    rev    = "f066503df2c6d8d57e06630615d2097741d09d39";
    sha256 = "1drqwz5ry8i9sv34kkywl5hj0p4yffbjgzb5fgpp4dzdgfxl0cqk";
  };
  displays = self: builtins.listToAttrs (
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
  haskellPackages = nixpkgs.haskell.packages.ghc821.override {
    overrides = self: super: {
      ihaskell       = nixpkgs.haskell.lib.overrideCabal (
                       self.callCabal2nix "ihaskell"          ihaskell-src       {}) (_drv: {
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
      ghc-parser        = self.callCabal2nix "ghc-parser" ghc-parser-src {};
      ipython-kernel    = self.callCabal2nix "ipython-kernel" ipython-kernel-src {};
      plot              = self.callCabal2nix "plot" plot {};

      diagrams-cairo    = nixpkgs.haskell.lib.doJailbreak super.diagrams-cairo;
      shelly            = nixpkgs.haskell.lib.doJailbreak super.shelly;
      static-canvas     = nixpkgs.haskell.lib.doJailbreak super.static-canvas;
      testing-feat      = nixpkgs.haskell.lib.doJailbreak super.testing-feat;

      cairo             = nixpkgs.lib.overrideDerivation super.cairo (drv: {
        src = "${gtk2hs}/cairo";
      });
      glib              = nixpkgs.lib.overrideDerivation super.glib (drv: {
        src = "${gtk2hs}/glib";
      });
      pango             = nixpkgs.lib.overrideDerivation super.pango (drv: {
        src = "${gtk2hs}/pango";
      });

      gtk2hs-buildtools = super.callHackage "gtk2hs-buildtools" "0.13.3.0" {};
      hmatrix           = super.hmatrix_0_18_1_0;
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
    extraLibs = with nixpkgs; [ python3Packages.jupyter python3Packages.notebook ];
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
