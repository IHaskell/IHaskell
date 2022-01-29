let
  nixpkgs-src = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/tarball/30d3d79b7d3607d56546dd2a6b49e156ba0ec634";
    sha256 = "0x5j9q1vi00c6kavnjlrwl3yy1xs60c34pkygm49dld2sgws7n0a";
  };
  overlays = [ (final: prev: {
    python3 = prev.python3.override {
      # Careful, we're using a different self and super here!
      packageOverrides = pfinal: pprev: {
        openapi-core = pprev.openapi-core.overridePythonAttrs(oa: {

          doCheck = false;
          doInstallCheck = false;
        });
      };
    };
  }) ];
in
{ compiler ? "ghc902"
, nixpkgs ? import nixpkgs-src { inherit overlays;}
, packages ? (_: [])
, pythonPackages ? (_: [])
, rtsopts ? "-M3g -N2"
, staticExecutable ? false
, systemPackages ? (_: [])
}:
  import (./release.nix) { inherit compiler nixpkgs packages pythonPackages rtsopts systemPackages; }
