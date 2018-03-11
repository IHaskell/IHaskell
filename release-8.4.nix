{ nixpkgs_ ? import <nixpkgs> {}, packages ? (_: []), rtsopts ? "-M3g -N2", systemPackages ? (_: []) }:

let
  nixpkgs = import (nixpkgs_.fetchFromGitHub {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "25f543a9f7444fe85320467464a04bf6ea926899";
    sha256 = "1w7g29iwhgixgbyj0l9cibdjvny0djqykzw9i148czh5v8bhnai1";
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
  plot = nixpkgs.fetchFromGitHub {
    owner  = "amcphail";
    repo   = "plot";
    rev    = "cc5cdff696aa99e1001124917c3b87b95529c4e3";
    sha256 = "13abrymry4nqyl9gmjrj8lhplbg4xag7x41n89yyw822360d3drh";
  };
  gtk2hs = nixpkgs.fetchFromGitHub {
    owner  = "gtk2hs";
    repo   = "gtk2hs";
    rev    = "ba28ee939b6321c89d11ac2480a5065b7c05a0ea";
    sha256 = "0cx9fs05c3sscywn4zg6g0kag64xyq7x4y38khir3516fgnj7zaa";
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
      hourglass         = super.hourglass.overrideAttrs (oldAttrs: {
        src = nixpkgs.fetchFromGitHub {
          owner  = "vincenthz";
          repo   = "hs-hourglass";
          rev    = "b9c4e2bc3a77454a10d5396ffd9f4147aa3ebc62";
          sha256 = "1rxai65xk6pcj3jahh62x543r1lgmzd4xjaj538nsb1aww0jyk77";
        };
      });
      regex-tdfa        = nixpkgs.haskell.lib.doJailbreak super.regex-tdfa;
      # plot              = self.callCabal2nix "plot" plot {};

      # diagrams-cairo    = nixpkgs.haskell.lib.doJailbreak super.diagrams-cairo;
      # shelly            = nixpkgs.haskell.lib.doJailbreak super.shelly;
      # static-canvas     = nixpkgs.haskell.lib.doJailbreak super.static-canvas;
      # testing-feat      = nixpkgs.haskell.lib.doJailbreak super.testing-feat;

      # cairo             = super.cairo.overrideAttrs (oldAttrs: {
      #   src = "${gtk2hs}/cairo";
      # });
      # glib              = super.glib.overrideAttrs (oldAttrs: {
      #   src = "${gtk2hs}/glib";
      # });
      # pango             = super.pango.overrideAttrs (oldAttrs: {
      #   src = "${gtk2hs}/pango";
      # });

      # gtk2hs-buildtools = super.callHackage "gtk2hs-buildtools" "0.13.3.0" {};
      # hmatrix           = super.hmatrix_0_18_1_0;
      # singletons        = super.callHackage "singletons" "2.3.1" {};
      # th-desugar        = super.callHackage "th-desugar" "1.7" {};
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
