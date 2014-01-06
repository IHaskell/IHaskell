#!/bin/sh
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..
cabal install --force-reinstalls || exit 1

# Remove my profile
rm -rf ~/.ipython/profile_haskell

cd ihaskell-display
for dir in `ls`
do
    cd $dir
    cabal install || exit 1
    cd ..
done
