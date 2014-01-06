#!/bin/sh
cd profile
rm profile.tar
tar -cvf profile.tar *
cd ..
cabal install --force-reinstalls || return 1
cd ihaskell-display
for dir in `ls`
do
    cd $dir
    cabal install || return 1
    cd ..
done

# Remove my profile
rm -rf ~/.ipython/profile_haskell
