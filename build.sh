#!/bin/bash
set -e

# Verify that we're in the IHaskell directory.
test -f "ihaskell.cabal"

# What to install.
INSTALLS=""

# Make the profile
cd profile
rm -f profile.tar
tar -cvf profile.tar *
cd ..

# Remove my profile
rm -rf ~/.ipython/profile_haskell

# Compile dependencies.
if [ $# -gt 0 ]; then
  if [ $1 = "all" ]; then
    INSTALLS="$INSTALLS ipython-kernel ghc-parser ghci-lib"
  fi
fi

# Make ihaskell itself
INSTALLS="$INSTALLS ."

# Install ihaskell-display packages.
if [ $# -gt 0 ]; then
  if [ $1 = "display" ]; then
        # Install all the display libraries
        cd ihaskell-display
        for dir in `ls`
        do
            INSTALLS="$INSTALLS ihaskell-display/$dir"
        done
        cd ..
    fi
fi

# Clean all required directories, just in case.
TOP=`pwd`
for pkg in $INSTALLS
do
    cd ./$pkg
    cabal clean
    cd $TOP
done

# Stick a "./" before everything.
INSTALL_DIRS=`echo $INSTALLS | tr ' ' '\n' | sed 's#^#./#' | tr ' ' '\n'`
cabal install -j $INSTALL_DIRS --force-reinstalls
