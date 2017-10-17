{ nixpkgs ? import <nixpkgs> {}, packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:

let
  inherit (builtins) any elem filterSource listToAttrs;
  cleanSource = name: type: let
    baseName = baseNameOf (toString name);
    lib = nixpkgs.lib;
  in lib.cleanSourceFilter name type && !(
    (type == "directory" && (elem baseName [ ".stack-work" "dist"])) ||
    any (lib.flip lib.hasSuffix baseName) [ ".hi" ".ipynb" ".nix" ".sock" ".yaml" ".yml" ]
  );
  src = filterSource cleanSource ./.;
  plot = nixpkgs.fetchFromGitHub {
    owner  = "amcphail";
    repo   = "plot";
    rev    = "cc5cdff696aa99e1001124917c3b87b95529c4e3";
    sha256 = "13abrymry4nqyl9gmjrj8lhplbg4xag7x41n89yyw822360d3drh";
  };
  displays = self: builtins.listToAttrs (
    map
      (display: { name = display; value = self.callCabal2nix display "${src}/ihaskell-display/${display}" {}; })
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
  stringToReplace   = "setSessionDynFlags\n      flags";
  replacementString = "setSessionDynFlags $ flip gopt_set Opt_BuildDynamicToo\n      flags";
  haskellPackages = nixpkgs.haskell.packages.ghc821.override {
    overrides = self: super: rec {
      ihaskell       = nixpkgs.haskell.lib.overrideCabal (
                       self.callCabal2nix "ihaskell"          src                  {}) (_drv: {
        doCheck = false;
        postPatch = ''
          substituteInPlace ./src/IHaskell/Eval/Evaluate.hs --replace \
            '${stringToReplace}' '${replacementString}'
        '';
      });
      ghc-parser        = self.callCabal2nix "ghc-parser"     "${src}/ghc-parser"     {};
      ipython-kernel    = self.callCabal2nix "ghc-parser"     "${src}/ipython-kernel" {};
      shelly = nixpkgs.haskell.lib.doJailbreak super.shelly;

      plot              = self.callCabal2nix "plot"               plot             { inherit cairo pango; };

      Chart             = super.callHackage "Chart" "1.8.2" {};
      Chart-cairo       = super.callHackage "Chart-cairo" "1.8.2" {};
      cairo             = super.callHackage "cairo" "0.13.3.0" {};
      cubicbezier       = super.callHackage "cubicbezier" "0.6.0.4" {};
      diagrams          = super.callHackage "diagrams" "1.4" {};
      diagrams-cairo    = super.callHackage "diagrams-cairo" "1.4" {};
      diagrams-contrib  = super.callHackage "diagrams-contrib" "1.4.1" {};
      diagrams-core     = super.callHackage "diagrams-core" "1.4.0.1" {};
      diagrams-lib      = super.callHackage "diagrams-lib" "1.4.1.2" {};
      diagrams-solve    = super.callHackage "diagrams-solve" "0.1.1" {};
      diagrams-svg      = super.callHackage "diagrams-svg" "1.4.1" {};
      dual-tree         = super.callHackage "dual-tree" "0.2.1" {};
      fast-math         = super.callHackage "fast-math" "1.0.2" {};
      glib              = super.callHackage "glib" "0.13.5.0"  {};
      gtk2hs-buildtools = super.callHackage "gtk2hs-buildtools" "0.13.2.2" {};
      magic             = super.callHackage "magic" "1.1" {};
      mfsolve           = super.callHackage "mfsolve" "0.3.2.0" {};
      pango             = super.callHackage "pango" "0.13.4.0" {};
      statestack        = super.callHackage "statestack" "0.2.0.5" {};
      static-canvas     = super.callHackage "static-canvas" "0.2.0.3" {};
      svg-builder       = super.callHackage "svg-builder" "0.1.0.2" {};
    } // displays self;
  };
  ihaskell = haskellPackages.ihaskell;
  ihaskellEnv = haskellPackages.ghcWithPackages (self: with self; [
    ihaskell
    ihaskell-aeson
    ihaskell-blaze
    # ihaskell-charts
    # ihaskell-diagrams
    ihaskell-gnuplot
    ihaskell-hatex
    ihaskell-juicypixels
    ihaskell-magic
    # ihaskell-plot
    # ihaskell-rlangqq
    # ihaskell-static-canvas
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
