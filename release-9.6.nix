let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/147886c1b7771e5f155beb2644545441737d450f";
    sha256 = "sha256:1k1rcdcjvphp989lg32kcjwdww46k0i59kf98k4f75hpgfq2j6bb";
  };
in
{ compiler ? "ghc961"
, nixpkgs ? import nixpkgs-src {}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:
let
  ihaskell-src = nixpkgs.nix-gitignore.gitignoreSource
    [ "**/*.ipynb" "**/*.nix" "**/*.yaml" "**/*.yml" "**/\.*" "/Dockerfile" "/README.md" "/cabal.project" "/images" "/notebooks" "/requirements.txt" ]
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
        rev = "71ff751eaa6034d4aef254d6bc5a8be4f6595344";
        sha256 = "14yahxi4pnjbvcd9r843kn7b36jsjaixd99jswsrh9n8xd59c2f1";
      };
      in
        self.callCabal2nix "ghc-syntax-highlighter" src {};
    ipython-kernel = self.callCabal2nix "ipython-kernel" (builtins.path { path = ./ipython-kernel; name = "ipython-kernel-src"; }) {};

    zeromq4-haskell = nixpkgs.haskell.lib.addPkgconfigDepend super.zeromq4-haskell nixpkgs.libsodium;
    here = nixpkgs.haskell.lib.appendPatch (nixpkgs.haskell.lib.doJailbreak super.here) (nixpkgs.fetchpatch {
      url = "https://github.com/tmhedberg/here/commit/3c648cdef8998383d9b63af4984ccb12c7729644.patch";
      sha256 = "sha256-Cvedt/UpH0tWrXVHCNFZlt0dr443IAkCOJdSjuPLIf8=";
    });
    shelly = nixpkgs.haskell.lib.doJailbreak super.shelly;
    hourglass = nixpkgs.haskell.lib.dontCheck super.hourglass;
  } // displays self);

  # statically linking against haskell libs reduces closure size at the expense
  # of startup/reload time, so we make it configurable
  ihaskellExe = if staticExecutable
    then nixpkgs.haskell.lib.justStaticExecutables haskellPackages.ihaskell
    else nixpkgs.haskell.lib.enableSharedExecutables haskellPackages.ihaskell;
  ihaskellEnv = haskellPackages.ghcWithPackages packages;
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ] ++ pythonPackages ps);
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
