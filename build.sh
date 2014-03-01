#!/bin/bash

set -e

INSTALLS="ipython-kernel"
# ipython-kernel
cd ipython-kernel
cabal clean
cd ..

# Make the profile
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..

if [ $# -gt 0 ]; then
  if [ $1 = "all" ]; then
    $INSTALLS="$INSTALLS ghc-parser ghci-lib"
  fi
fi

# Make ihaskell itself
cabal clean

cabal install --force-reinstalls $INSTALLS .

# Remove my profile
rm -rf ~/.ipython/profile_haskell

if [ $# -gt 0 ]; then
  if [ $1 = "display" ]; then
        # Install all the display libraries
        cd ihaskell-display
        for dir in `ls`
        do
            cd $dir
            cabal clean
            cd ..
        done
        cabal install *
    fi
fi
