let
  fetcher = { owner, repo, rev, sha256 }: builtins.fetchTarball {
    inherit sha256;
    url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
  };
  head-hackage = fetcher {
    owner = "mpickering";
    repo = "head.hackage";
    rev = "d26e4c7ad9e2bd6419338c644b1393f41109a9fa";
    sha256 = "0vr2pl7yhfjik8q5ayak97xkkdr2d5anjl1s5p60nhg83r034pfc";
  };
in
{ compiler ? "ghc861"
, nixpkgs ? import head-hackage {}
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
  th-lift-src = fetcher {
    owner = "mboes";
    repo = "th-lift";
    rev = "8969cfb5f670d09d7d18b1e55325ada82c300ddd";
    sha256 = "1cd55zh6iagc040w65yppcg9d9sxhxy4jn9qhd2l56wc422ywcb2";
  };
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
  haskellPackages = nixpkgs.haskellPackages.extend (self: super: {
    ihaskell          = nixpkgs.haskell.lib.overrideCabal (
                        self.callCabal2nix "ihaskell" ihaskell-src {}) (_drv: {
      preCheck = ''
        export HOME=$(${nixpkgs.pkgs.coreutils}/bin/mktemp -d)
        export PATH=$PWD/dist/build/ihaskell:$PATH
        export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
      '';
      doCheck = true;
      configureFlags = (_drv.configureFlags or []) ++ [
        # otherwise the tests are agonisingly slow and the kernel times out
        "--enable-executable-dynamic"
      ];
    });
    ghc-parser        = self.callCabal2nix "ghc-parser" ghc-parser-src {};
    ipython-kernel    = self.callCabal2nix "ipython-kernel" ipython-kernel-src {};
    th-lift           = super.callCabal2nix "th-lift" th-lift-src {};
  } // displays self);
  ihaskellEnv = haskellPackages.ghcWithPackages (self: [ self.ihaskell ] ++ packages self);
  jupyter = nixpkgs.python3.withPackages (ps: [ ps.notebook ] ++ pythonPackages ps);
  ihaskellSh = cmd: extraArgs: nixpkgs.writeScriptBin "ihaskell-${cmd}" ''
    #! ${nixpkgs.stdenv.shell}
    export GHC_PACKAGE_PATH="$(echo ${ihaskellEnv}/lib/*/package.conf.d| tr ' ' ':'):$GHC_PACKAGE_PATH"
    export PATH="${nixpkgs.stdenv.lib.makeBinPath ([ ihaskellEnv jupyter ] ++ systemPackages nixpkgs)}:$PATH"
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
