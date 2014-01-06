#!/bin/sh
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..
cabal install --force-reinstalls || exit 1
cd ihaskell-display
for dir in `ls`
do
    cd $dir
    cabal install || exit 1
    cd ..
done

# Remove my profile
rm -rf ~/.ipython/profile_haskell
