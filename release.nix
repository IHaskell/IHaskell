{ packages ? (_: []), systemPackages ? (_: []), pkgs ? import <nixpkgs> {}, rtsopts ? "-M3g -N2" }:

let
  cleanSource = name: type: let
      baseName = baseNameOf (toString name);
      lib = pkgs.lib;
    in !(
      (type == "directory" &&
        (  baseName == ".git"
        || baseName == "dist"
        || baseName == ".stack-work"
      ))                                                          ||
      (type == "symlink"   && (lib.hasPrefix "result" baseName))  ||
      lib.hasSuffix ".hi"    baseName                             ||
      lib.hasSuffix ".ipynb" baseName                             ||
      lib.hasSuffix ".nix"   baseName                             ||
      lib.hasSuffix ".o"     baseName                             ||
      lib.hasSuffix ".sock"  baseName                             ||
      lib.hasSuffix ".yaml"  baseName
    );
  src = builtins.filterSource cleanSource ./.;
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
  dontCheck = pkgs.haskell.lib.dontCheck;
  stringToReplace   = "setSessionDynFlags\n      flags";
  replacementString = "setSessionDynFlags $ flip gopt_set Opt_BuildDynamicToo\n      flags";
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      ihaskell       = pkgs.haskell.lib.overrideCabal (
                       self.callCabal2nix "ihaskell"          src                  {}) (_drv: {
        doCheck = false;
        postPatch = ''
          substituteInPlace ./src/IHaskell/Eval/Evaluate.hs --replace \
            '${stringToReplace}' '${replacementString}'
        '';
      });
      ghc-parser     = self.callCabal2nix "ghc-parser"     "${src}/ghc-parser"     {};
      ipython-kernel = self.callCabal2nix "ipython-kernel" "${src}/ipython-kernel" {};
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
  jupyter = pkgs.python3.buildEnv.override {
    extraLibs = let ps = pkgs.python3Packages; in [ ps.jupyter ps.notebook ];
  };
  ihaskellSh = pkgs.writeScriptBin "ihaskell-notebook" ''
    #! ${pkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${pkgs.stdenv.lib.makeBinPath ([ ihaskell ihaskellEnv jupyter ] ++ systemPackages pkgs)}"
    ${ihaskell}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) --use-rtsopts="${rtsopts}" && ${jupyter}/bin/jupyter notebook
  '';
in
pkgs.buildEnv {
  name = "ihaskell-with-packages";
  buildInputs = [ pkgs.makeWrapper ];
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
