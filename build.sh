#!/bin/sh
set -e

function print_help {
  echo "Run build.sh from inside the IHaskell directory to install packages in this repository:"
  echo "  ./build.sh ihaskell # Install IHaskell and its dependencies"
  echo "  ./build.sh quick    # Install IHaskell, but not its dependencies"
  echo "  ./build.sh all      # Install IHaskell, dependencies, and all display packages"
  echo "  ./build.sh display  # Install IHaskell and display libraries"
  echo
  echo "If this is your first time installing IHaskell, run './build.sh ihaskell'."
}

# Verify that we're in the IHaskell directory.
if [ ! -e ihaskell.cabal ]; then
  print_help;
  exit 1
fi

if [ $# -lt 1 ]; then
    print_help;
    exit 1
fi

if [ ! $1 = "all" ] && [ ! $1 = "ihaskell" ] && [ ! $1 = "display" ] && [ ! $1 = "quick" ]; then
    print_help;
    exit 1;
fi

# What to install.
INSTALLS=""

# Remove my kernelspec
rm -rf ~/.ipython/kernels/haskell

# Compile dependencies.
if [ $# -gt 0 ]; then
  if [ $1 = "all" ] || [ $1 = "ihaskell" ]; then
    INSTALLS="$INSTALLS ghc-parser ipython-kernel"
  fi
fi

# Always make ihaskell itself
INSTALLS="$INSTALLS ."

# Install ihaskell-display packages.
if [ $# -gt 0 ]; then
  if [ $1 = "display" ] || [ $1 = "all" ]; then
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

if [ `uname` = Darwin ]; then
  echo CMD: cabal install --constraint "arithmoi -llvm" -j $INSTALL_DIRS --force-reinstalls
  cabal install --constraint "arithmoi -llvm" -j $INSTALL_DIRS --force-reinstalls
else
  echo CMD: cabal install -j $INSTALL_DIRS --force-reinstalls --constraint "arithmoi==0.4.*"
  cabal install -j $INSTALL_DIRS --force-reinstalls --constraint "arithmoi==0.4.*"
fi
