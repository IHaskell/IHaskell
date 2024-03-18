{
  description = "A Haskell kernel for IPython.";

  inputs.nixpkgs23_11.url = "github:NixOS/nixpkgs/release-23.11";
  inputs.nixpkgsMaster.url = "github:NixOS/nixpkgs/master";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.hls = {
    url = "github:haskell/haskell-language-server";
    inputs.flake-utils.follows = "flake-utils";
  };
  inputs.nix-filter.url = "github:numtide/nix-filter";

  nixConfig = {
    extra-substituters = [ "https://ihaskell.cachix.org" ];
    extra-trusted-public-keys = [ "ihaskell.cachix.org-1:WoIvex/Ft/++sjYW3ntqPUL3jDGXIKDpX60pC8d5VLM="];
  };

  outputs = { self, nixpkgs23_11, nixpkgsMaster, flake-utils, hls, nix-filter, ... }:
    flake-utils.lib.eachDefaultSystem (system: let
      baseOverlay = self: super: { inherit nix-filter; };
      pkgs23_11 = import nixpkgs23_11 { inherit system; overlays = [baseOverlay]; };
      pkgsMaster = import nixpkgsMaster { inherit system; overlays = [baseOverlay]; };

      jupyterlab = pkgsMaster.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);

      # Map from GHC version to release function
      versions = let
        mkVersion = pkgsSrc: compiler: overlays: extraArgs: {
          name = compiler;
          value = (import pkgsSrc { inherit system; overlays = [baseOverlay] ++ overlays; }).callPackage ./nix/release.nix ({
            inherit compiler;
          } // extraArgs);
        };
        in
          pkgsMaster.lib.listToAttrs [
            (mkVersion nixpkgs23_11  "ghc94"  [(import ./nix/overlay-9.4.nix)]  {})
            (mkVersion nixpkgsMaster "ghc96"  [(import ./nix/overlay-9.6.nix)]  {})
            (mkVersion nixpkgsMaster "ghc98"  [(import ./nix/overlay-9.8.nix)]  { enableHlint = false; })
          ];

      # Helper function for building environments with a given set of packages
      mkEnvs = prefix: packages: pkgsMaster.lib.mapAttrs' (version: releaseFn: {
        name = prefix + version;
        value = (releaseFn {
          # Note: this can be changed to other Jupyter systems like jupyter-console
          extraEnvironmentBinaries = [ jupyterlab ];
          systemPackages = p: with p; [
            gnuplot # for the ihaskell-gnuplot runtime
          ];
          inherit packages;
        });
      }) versions;

      # Basic envs with Jupyterlab and IHaskell
      envs = mkEnvs "ihaskell-env-" (_: []);

      # Envs with Jupyterlab, IHaskell, and all display packages
      displayEnvs = mkEnvs "ihaskell-env-display-" (p: with p; map (n: builtins.getAttr n p) (import ./nix/displays.nix));

      # Executables only, pulled from passthru on the envs
      exes = pkgsMaster.lib.mapAttrs' (envName: env: {
        name = builtins.replaceStrings ["-env"] [""] envName;
        value = env.ihaskellExe;
      }) envs;

      devShells = pkgsMaster.lib.mapAttrs' (version: releaseFn: {
        name = "ihaskell-dev-" + version;
        value = pkgsMaster.callPackage ./nix/mkDevShell.nix {
          inherit hls system version;
          haskellPackages = (releaseFn {}).haskellPackages;
          ihaskellOverlay = (releaseFn {}).ihaskellOverlay;
        };
      }) versions;

    in {
      packages = envs // displayEnvs // exes // devShells // rec  {
        # For easily testing that everything builds
        allEnvs = pkgsMaster.linkFarm "ihaskell-envs" envs;
        allDisplayEnvs = pkgsMaster.linkFarm "ihaskell-display-envs" displayEnvs;
        allExes = pkgsMaster.linkFarm "ihaskell-exes" exes;
        allDevShells = pkgsMaster.linkFarm "ihaskell-dev-shells" devShells;

        # To use in CI
        inherit jupyterlab;
        print-nixpkgs-master = pkgsMaster.writeShellScriptBin "print-nixpkgs-master.sh" "echo ${pkgsMaster.path}";
      };

      # Run the acceptance tests on each env
      checks = pkgsMaster.lib.mapAttrs (envName: env:
        pkgsMaster.stdenv.mkDerivation {
          name = envName + "-check";
          src = pkgsMaster.callPackage ./nix/ihaskell-src.nix {};
          nativeBuildInputs = with pkgsMaster; [jq bash];
          doCheck = true;
          checkPhase = ''
            mkdir -p home
            export HOME=$(pwd)/home
            bash ./test/acceptance.nbconvert.sh ${env}/bin/jupyter nbconvert
          '';
          installPhase = ''
            touch $out
          '';
        }
      ) envs;

      defaultPackage = self.packages.${system}.ihaskell-env-ghc94;

      devShell = self.packages.${system}.ihaskell-dev-ghc94;
    });
}
