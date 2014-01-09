#!/bin/sh

# Recompile ipython-kernel
cd ipython-kernel
cabal clean
cabal install --force-reinstalls || exit 1
cd ..

# Make the profile
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..

# Make ihaskell itself
cabal clean
cabal install --force-reinstalls || exit 1

# Remove my profile
rm -rf ~/.ipython/profile_haskell

# Install all the display libraries
cd ihaskell-display
for dir in `ls`
do
    cd $dir
    cabal clean
    cabal install || exit 1
    cd ..
done
