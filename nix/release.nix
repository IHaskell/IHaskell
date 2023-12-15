{ lib
, buildEnv
, callPackage
, haskell
, makeWrapper
, pkgs
, runCommand
, writeShellScriptBin

# Compiler name as a string, like "ghc92".
# Must be a kernel found within pkgs.haskell.packages.*.
, compiler
# Whether to enable hlint.
, enableHlint ? true
}:

{
# Extra binaries to include in the final environment, like jupyter-lab or jupyter-console.
# Will have their JUPYTER_PATH and PATH environment variables prefixed to tell them about the kernel.
extraEnvironmentBinaries ? []
# Haskell packages to include. First argument is an attrset of available packages.
, packages ? (_: [])
# RTS options passed when invoking IHaskell in the kernelspec.
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:

let
  ihaskellOverlay = callPackage ./ihaskell-overlay.nix { inherit enableHlint; };

  # Haskell packages set with IHaskell packages added
  haskellPackages = haskell.packages."${compiler}".override (old: {
    overrides = lib.composeExtensions
      (old.overrides or (_: _: {}))
      ihaskellOverlay;
  });

  # GHC with desired packages. This includes user-configured packages plus IHaskell itself, so
  # you can import things like IHaskell.Display
  ihaskellEnv = haskellPackages.ghcWithPackages (ps: (packages ps) ++ [ps.ihaskell]);

  # ihaskell binary wrapper which adds the "-l" argument
  ihaskellGhcLib = writeShellScriptBin "ihaskell" ''
    ${ihaskellEnv}/bin/ihaskell -l $(${ihaskellEnv}/bin/ghc --print-libdir) "$@"
  '';

  # Jupyter directory with kernels/haskell/kernel.json, plus logo and kernel.js
  jupyterDirKernel = let
    kernelFile = {
      display_name = "Haskell";
      argv = [
        "${ihaskellGhcLib}/bin/ihaskell"
        "kernel"
        "{connection_file}"
        "+RTS"
      ] ++ (lib.splitString " " rtsopts) ++ [
        "-RTS"
      ];
      language = "haskell";
    };
  in
    runCommand "ihaskell-kernel" {} ''
      export kerneldir=$out/kernels/haskell
      mkdir -p $kerneldir
      cp ${../html}/* $kerneldir
      echo '${builtins.toJSON kernelFile}' > $kerneldir/kernel.json
    '';

  # Separate Jupyter directory with the "labextensions" dir.
  # TODO: just copy this alongside the HTML in jupyterDir?
  jupyterDirLabExtensions = runCommand "ihaskell-labextension" {} ''
    mkdir -p $out/labextensions/
    ln -s ${../jupyterlab-ihaskell/labextension} $out/labextensions/jupyterlab-ihaskell
  '';

  # Combine the paths in jupyterDirKernel and jupyterDirLabExtensions
  ihaskellDataDir = buildEnv {
    name = "ihaskell-data-dir-" + compiler;
    paths = [ jupyterDirKernel jupyterDirLabExtensions ];
  };

in

# Final IHaskell environment:
buildEnv {
  name = "ihaskell-with-packages-" + compiler;
  nativeBuildInputs = [ makeWrapper ];
  paths = [ ihaskellEnv ] ++ extraEnvironmentBinaries;
  postBuild = ''
    for prg in $out/bin"/"*;do
      if [[ -f $prg && -x $prg ]]; then
        wrapProgram $prg \
          --prefix PATH : "${lib.makeBinPath ([ihaskellEnv] ++ (systemPackages pkgs))}" \
          --prefix JUPYTER_PATH : "${ihaskellDataDir}"
      fi
    done
  '';

  passthru = {
    inherit haskellPackages;
    inherit ihaskellOverlay;
    # statically linking against haskell libs reduces closure size at the expense
    # of startup/reload time, so we make it configurable
    ihaskellExe = if staticExecutable
                  then haskell.lib.justStaticExecutables haskellPackages.ihaskell
                  else haskell.lib.enableSharedExecutables haskellPackages.ihaskell;
  };

  meta = {
    mainProgram = "jupyter-lab";
  };
}
