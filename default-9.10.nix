let
  nixpkgs-src = ../nixpkgs;
  nix-filter-src = builtins.fetchTarball {
    url = "https://github.com/numtide/nix-filter/tarball/3342559a24e85fc164b295c3444e8a139924675b";
    sha256 = "sha256:08xia32g5jzaiyhfl3kzxslzbr4w1i56i0mhy5gpfggv8gqy8sym";
  };
in
let
  overlay = sel: sup: { nix-filter = import nix-filter-src; };
  haskell-overlay = import ./nix/overlay-9.10.nix;
  nixpkgs = import nixpkgs-src { system = builtins.currentSystem; overlays = [ overlay haskell-overlay ]; };
  jupyterlab = nixpkgs.python3.withPackages (ps: [ ps.jupyterlab ps.notebook ]);
in nixpkgs.callPackage ./nix/release.nix { compiler = "ghc910"; enableHlint = false; runTests = false;}{
  extraEnvironmentBinaries = [jupyterlab];
  packages = self: with self; [];
}
