#!/bin/bash
set -e

# Verify that we're in the IHaskell directory.
if [ ! -e ihaskell.cabal ]; then
  echo "Run build.sh from inside the IHaskell directory:"
  echo "  ./build.sh all      # Install IHaskell and deps (use if first install)"
  echo "  ./build.sh          # Install only IHaskell, no deps"
  echo "  ./build.sh display  # Install IHaskell and display libraries"
  exit 1
fi

# What to install.
INSTALLS=""

# Remove my kernelspec
rm -rf ~/.ipython/kernels/haskell

# Compile dependencies.
if [ $# -gt 0 ]; then
  if [ $1 = "all" ]; then
    INSTALLS="$INSTALLS ghc-parser ipython-kernel"
  fi
fi

# Make ihaskell itself
INSTALLS="$INSTALLS ."

# Install ihaskell-display packages.
if [ $# -gt 0 ]; then
  if [ $1 = "display" ]; then
        # Install all the display libraries
        # However, install ihaskell-diagrams separately...
        cd ihaskell-display
        for dir in `ls | grep -v diagrams`
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

# Finish installing ihaskell-diagrams.
if [ $# -gt 0 ]; then
  if [ $1 = "display" ]; then
      cabal install -j ihaskell-display/ihaskell-diagrams --force-reinstalls
    fi
fi
