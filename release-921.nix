let
  lock = builtins.fromJSON (builtins.readFile ./flake.lock);

  compat = (import
    (
      let lockedCompat = lock.nodes.flake-compat.locked;
      in
      fetchTarball {
        url =
          "https://github.com/${lockedCompat.owner}/${lockedCompat.repo}/archive/${lockedCompat.rev}.tar.gz";
        sha256 = lockedCompat.narHash;
      }
    )
    { src = ./.; });
in
{ ... }: compat.result.packages.${builtins.currentSystem}.ihaskell-921
