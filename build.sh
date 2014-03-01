#!/bin/bash
set -e

# Verify that we're in the IHaskell directory.
test -f "ihaskell.cabal"

# get cabal cabal-meta
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

declare m4Args
# Compile dependencies.
if [ $# -gt 0 ]; then
  m4Args[${#m4Args}]=-D_ALL
  if [ $1 = "all" ]; then
    m4Args[${#m4Args}]=-D_DISP
  fi
fi

m4 ${m4Args[*]} < sources.txt.in > sources.txt
cabal-meta install
