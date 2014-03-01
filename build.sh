#!/bin/bash
set -e

# Verify that we're in the IHaskell directory.
if [ ! -e ihaskell.cabal ]; then
  echo "run from inside the IHaskell directory:
        ./build.sh all
        ./build.sh    # less dependencies"
  exit 1
fi

# Get cabal cabal-meta if necessary.
[ -e "`which cabal-meta`" ] || cabal install cabal-meta cabal-src
if [ ! -e "`which cabal-meta`" ]; then
  echo "cabal-meta isn't in \$PATH:\n
    maybe you need to add $HOME/.cabal/bin to your \$PATH";
  exit 1
fi

# Make the profile
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..

# Remove my profile
rm -rf ~/.ipython/profile_haskell

# Compile dependencies.
declare -a args
[ $# = 0 ] || args[${#args[*]}]=-D_ALL
[ "$1" != "all" ] || args[${#args[*]}]=-D_DISP
[ -d vendor ] || args[${#args[*]}]=-D_PULL

cpp -P -C ${args[*]} < sources.txt.in > sources.txt

if [ ${#args} -lt 1 ]; then
  force=--force
fi
cabal-meta install $force
