let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/cb8c0b6ac06c43f9e2745ffe8050cd5b86663a25";
    sha256 = "sha256:1bklywsl84c2y6lyfjn7zxbwajibdlh13l14z6z5g9c418rgr3wy";
  };
in
{ compiler ? "ghc981"
, nixpkgs ? import nixpkgs-src {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:
let
  ihaskell-src = nixpkgs.nix-gitignore.gitignoreSource
    [ "**/*.ipynb" "**/*.nix" "**/*.yaml" "**/*.yml" "**/\.*" "/Dockerfile" "/README.md" "/cabal.project" "/images" "/notebooks" "/test" "/requirements.txt" ]
    ./.;
  displays = self: builtins.listToAttrs (
    map
      (display: { name = "ihaskell-${display}"; value = self.callCabal2nix display "${ihaskell-src}/ihaskell-display/ihaskell-${display}" {}; })
      [ "aeson" "blaze" "charts" "diagrams" "gnuplot" "graphviz" "hatex" "juicypixels" "magic" "plot" "rlangqq" "static-canvas" "widgets" ]);
  haskellPackages = nixpkgs.haskell.packages."${compiler}".override (old: {
    overrides = nixpkgs.lib.composeExtensions (old.overrides or (_: _: {})) ihaskellOverlay;
  });

  ihaskellOverlay = (self: super: {
    ihaskell = (nixpkgs.haskell.lib.overrideCabal (
                     self.callCabal2nix "ihaskell" ihaskell-src {}) (_drv: {
      preCheck = ''
        export HOME=$TMPDIR/home
        export PATH=$PWD/dist/build/ihaskell:$PATH
        export GHC_PACKAGE_PATH=$PWD/dist/package.conf.inplace/:$GHC_PACKAGE_PATH
      '';
      configureFlags = (_drv.configureFlags or []) ++ [ "-f" "-use-hlint" ];
    })).overrideScope (self: super: {
      hlint = null;
    });
    ghc-parser     = self.callCabal2nix "ghc-parser" (builtins.path { path = ./ghc-parser; name = "ghc-parser-src"; }) {};
    ghc-syntax-highlighter = let
      src = nixpkgs.fetchFromGitHub {
        owner = "mrkkrp";
        repo = "ghc-syntax-highlighter";
        rev = "336df42e9185c2e2a91fa71eaa18b51f0f5437de";
        sha256 = "0nnpyq3z6c90z3j8xzw2gcj13nhihc7xa0sb4xbbdpxfnidplp9q";
      };
      in
        self.callCabal2nix "ghc-syntax-highlighter" src {};
    ipython-kernel = self.callCabal2nix "ipython-kernel" (builtins.path { path = ./ipython-kernel; name = "ipython-kernel-src"; }) {};

    alex = nixpkgs.haskell.lib.dontCheck super.alex;
    # zeromq4-haskell = nixpkgs.haskell.lib.addPkgconfigDepend super.zeromq4-haskell nixpkgs.libsodium;
    ghc-lib-parser = nixpkgs.haskell.lib.overrideCabal (self.callHackage "ghc-lib-parser" "9.8.1.20231009" {}) (_drv: {
      postPatch = let
        original-a = "cabal-version: 2.0";
        replacement-a = "cabal-version: 3.0";
        original-b = "license: BSD3";
        replacement-b = "license: BSD-3-Clause";
        original-c =
          "c-sources:\r\n        libraries/ghc-heap/cbits/HeapPrim.cmm";
        replacement-c =
          "cmm-sources:\r\n        libraries/ghc-heap/cbits/HeapPrim.cmm";
      in ''
        substituteInPlace ./ghc-lib-parser.cabal --replace \
          '${original-a}' '${replacement-a}'
        substituteInPlace ./ghc-lib-parser.cabal --replace \
          '${original-b}' '${replacement-b}'
        substituteInPlace ./ghc-lib-parser.cabal --replace \
          '${original-c}' '${replacement-c}'
      '';
    });
    shelly = nixpkgs.haskell.lib.doJailbreak super.shelly;
    hourglass = nixpkgs.haskell.lib.dontCheck super.hourglass;
    unliftio-core = nixpkgs.haskell.lib.doJailbreak super.unliftio-core;
    tagged = self.callHackage "tagged" "0.8.8" {};
    th-abstraction = self.callHackage "th-abstraction" "0.6.0.0" {};
    th-lift = self.callHackage "th-lift" "0.8.4" {};
    hspec-core = nixpkgs.haskell.lib.dontCheck (self.callHackage "hspec-core" "2.11.6" {});
    hspec-expectations = self.callHackage "hspec-expectations" "0.8.4" {};
    hspec-meta = self.callHackage "hspec-meta" "2.11.6" {};
    hspec-discover = self.callHackage "hspec-discover" "2.11.6" {};
    hspec = self.callHackage "hspec" "2.11.6" {};
    lifted-base = nixpkgs.haskell.lib.dontCheck super.lifted-base;
    bifunctors = self.callHackage "bifunctors" "5.6.1" {};
    doctest = self.callHackage "doctest" "0.22.2" {};
    semigroupoids = self.callHackage "semigroupoids" "6.0.0.1" {};
    here = nixpkgs.haskell.lib.doJailbreak (self.callHackage "here" "1.2.14" {});
    aeson = self.callHackage "aeson" "2.2.1.0" {};
    haskell-src-meta = nixpkgs.haskell.lib.appendPatch (nixpkgs.haskell.lib.doJailbreak super.haskell-src-meta) (nixpkgs.fetchpatch {
      url = "https://gitlab.haskell.org/ghc/head.hackage/-/raw/master/patches/haskell-src-meta-0.8.12.patch";
      sha256 = "0wn0lnq522bhc65sw02jm448mg5ijdxwgs7gzp9xp6z8ir1a0bzr";
    });

  } // displays self);

  # statically linking against haskell libs reduces closure size at the expense
  # of startup/reload time, so we make it configurable
  ihaskellExe = if staticExecutable
    then nixpkgs.haskell.lib.justStaticExecutables haskellPackages.ihaskell
    else nixpkgs.haskell.lib.enableSharedExecutables haskellPackages.ihaskell;
  ihaskellEnv = haskellPackages.ghcWithPackages packages;
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ] ++ pythonPackages ps);
  ihaskellGhcLibFunc = exe: env: nixpkgs.writeShellScriptBin "ihaskell" ''
    ${exe}/bin/ihaskell -l $(${env}/bin/ghc --print-libdir) "$@"
  '';
  ihaskellKernelFileFunc = ihaskellGhcLib: rtsopts: {
    display_name = "Haskell";
    argv = [
      "${ihaskellGhcLib}/bin/ihaskell"
      "kernel"
      "{connection_file}"
      "+RTS"
    ] ++ (nixpkgs.lib.splitString " " rtsopts) ++ [
      "-RTS"
    ];
    language = "haskell";
  };
  ihaskellKernelSpecFunc = ihaskellKernelFile: nixpkgs.runCommand "ihaskell-kernel" {} ''
    export kerneldir=$out/kernels/haskell
    mkdir -p $kerneldir
    cp ${./html}/* $kerneldir
    echo '${builtins.toJSON ihaskellKernelFile}' > $kerneldir/kernel.json
  '';
  ihaskellLabextension = nixpkgs.runCommand "ihaskell-labextension" {} ''
    mkdir -p $out/labextensions/
    ln -s ${./jupyterlab-ihaskell/labextension} $out/labextensions/jupyterlab-ihaskell
  '';
  ihaskellDataDirFunc = ihaskellKernelSpec: ihaskellLabextension: nixpkgs.buildEnv {
    name = "ihaskell-data-dir";
    paths = [ ihaskellKernelSpec ihaskellLabextension ];
  };
  ihaskellBuildEnvFunc = { ihaskellEnv, jupyterlab, systemPackages, ihaskellDataDir }: nixpkgs.buildEnv {
    name = "ihaskell-with-packages";
    nativeBuildInputs = [ nixpkgs.makeWrapper ];
    paths = [ ihaskellEnv jupyterlab ];
    postBuild = ''
      for prg in $out/bin"/"*;do
        if [[ -f $prg && -x $prg ]]; then
          wrapProgram $prg \
            --prefix PATH : "${nixpkgs.lib.makeBinPath ([ihaskellEnv] ++ (systemPackages nixpkgs))}" \
            --prefix JUPYTER_PATH : "${ihaskellDataDir}"
        fi
      done
    '';
    passthru = {
      inherit haskellPackages;
      inherit ihaskellExe;
      inherit ihaskellEnv;
      inherit ihaskellOverlay;
      inherit ihaskellLabextension;
      inherit jupyterlab;
      inherit ihaskellGhcLibFunc;
      inherit ihaskellKernelFileFunc;
      inherit ihaskellKernelSpecFunc;
      inherit ihaskellDataDirFunc;
      inherit ihaskellBuildEnvFunc;
    };
  };
in ihaskellBuildEnvFunc {
  inherit ihaskellEnv jupyterlab systemPackages;
  ihaskellDataDir = let
    ihaskellGhcLib = ihaskellGhcLibFunc ihaskellExe ihaskellEnv;
    ihaskellKernelFile = ihaskellKernelFileFunc ihaskellGhcLib rtsopts;
    ihaskellKernelSpec = ihaskellKernelSpecFunc ihaskellKernelFile;
  in ihaskellDataDirFunc ihaskellKernelSpec ihaskellLabextension;
}
