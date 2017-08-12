{ packages ? (_: []), systemPackages ? (_: []), nixpkgs ? <nixpkgs> }:

let
  pkgs = import nixpkgs {};
  src = pkgs.lib.cleanSource ./.;
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
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      ihaskell       = dontCheck (
                          self.callCabal2nix "ihaskell"          src                                   { bin-package-db = null; });
      ghc-parser        = self.callCabal2nix "ghc-parser"     "${src}/ghc-parser"                      {};
      ipython-kernel    = self.callCabal2nix "ghc-parser"     "${src}/ipython-kernel"                  {};
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
    extraLibs = with pkgs.python3Packages; [ ipython ipykernel jupyter_client notebook ];
  };
  ihaskellSh = pkgs.writeScriptBin "ihaskell-notebook" ''
    #! ${pkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${pkgs.stdenv.lib.makeBinPath ([ ihaskell ihaskellEnv jupyter ] ++ systemPackages pkgs)}"
    ${ihaskell}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) && ${jupyter}/bin/jupyter notebook
  '';
  profile = "${ihaskell.pname}-${ihaskell.version}/profile/profile.tar";
in
pkgs.buildEnv {
  name = "ihaskell-with-packages";
  paths = [ ihaskellEnv jupyter ];
  postBuild = ''
    . "${pkgs.makeWrapper}/nix-support/setup-hook"
    ln -s ${ihaskellSh}/bin/ihaskell-notebook $out/bin/
    for prg in $out/bin"/"*;do
      wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
    done
  '';
}
