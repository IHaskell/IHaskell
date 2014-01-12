#!/bin/bash

set -e

# Recompile ipython-kernel
cd ipython-kernel
cabal clean
cabal install --force-reinstalls
cd ..

# Make the profile
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..

# Make ihaskell itself
cabal clean
cabal install --force-reinstalls

# Remove my profile
rm -rf ~/.ipython/profile_haskell

if [ $# -gt 0 ]; then
  if [ $1 = "all" ]; then
        # Install all the display libraries
        cd ihaskell-display
        for dir in `ls`
        do
            cd $dir
            cabal clean
            cabal install
            cd ..
        done
    fi
fi
