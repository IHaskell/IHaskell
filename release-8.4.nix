{ nixpkgs_ ? import <nixpkgs> {}, packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:

let
  nixpkgs = import (nixpkgs_.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "d843d7f21a66ba0a2f6517428815dc298ab14cb1";
    sha256 = "1h9lxv0768kg4sgkbcnzpfgsa928rxxzlbxfbl9psjaq20c2a6y1";
  }) {};
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
  haskellPackages = nixpkgs.haskell.packages.ghc841.override {
    overrides = self: super: {
      ihaskell          = nixpkgs.haskell.lib.overrideCabal (
                          self.callCabal2nix "ihaskell" ihaskell-src {}) (_drv: {
        preCheck = ''
          export HOME=$(${nixpkgs.pkgs.coreutils}/bin/mktemp -d)
          export PATH=$PWD/dist/build/ihaskell:$PATH
          export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
        '';
      });
      ghc-parser        = self.callCabal2nix "ghc-parser" ghc-parser-src {};
      ipython-kernel    = self.callCabal2nix "ipython-kernel" ipython-kernel-src {};

      hlint             = super.hlint.overrideScope (_self: _super: { haskell-src-exts = self.haskell-src-exts; });
    } // displays self;
  };
  ihaskellEnv = haskellPackages.ghcWithPackages (self: [ self.ihaskell ] ++ packages self);
  jupyter = nixpkgs.python3.withPackages (ps: [ ps.jupyter ps.notebook ]);
  ihaskellSh = cmd: extraArgs: nixpkgs.writeScriptBin "ihaskell-${cmd}" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv jupyter ] ++ systemPackages nixpkgs)}"
    ${ihaskellEnv}/bin/ihaskell install -l $(${ihaskellEnv}/bin/ghc --print-libdir) --use-rtsopts="${rtsopts}" && ${jupyter}/bin/jupyter ${cmd} ${extraArgs} "$@"
  '';
in
nixpkgs.buildEnv {
  name = "ihaskell-with-packages";
  buildInputs = [ nixpkgs.makeWrapper ];
  paths = [ ihaskellEnv jupyter ];
  postBuild = ''
    ln -s ${ihaskellSh "notebook" ""}/bin/ihaskell-notebook $out/bin/
    ln -s ${ihaskellSh "nbconvert" ""}/bin/ihaskell-nbconvert $out/bin/
    ln -s ${ihaskellSh "console" "--kernel=haskell"}/bin/ihaskell-console $out/bin/
    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        wrapProgram $prg --set PYTHONPATH "$(echo ${jupyter}/lib/*/site-packages)"
      fi
    done
  '';
}
