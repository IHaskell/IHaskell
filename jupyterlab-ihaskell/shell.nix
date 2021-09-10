let nixpkgs = import <nixpkgs> {};
in nixpkgs.mkShell { buildInputs = [ (nixpkgs.python3.withPackages (p: [ p.jupyterlab ] )) nixpkgs.nodejs ]; }
