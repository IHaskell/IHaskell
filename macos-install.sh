#!/bin/sh

# Installation for Mac OS X.
#
# This script assumes use of Homebrew.
# It is assumed you already have GHC and Cabal installed through Homebrew
# and your environment set up to install executables through Cabal.
#
# Also, XCode command line tools must be installed.
#
# XQuartz is needed for Cairo and Pango.

brew --version >& /dev/null
if [ $? ]; then
    true
else
    echo "Homebrew needs to be installed."
    echo "  Download from http://brew.sh/"
    exit 1
fi

if [ -n "`brew --config | grep '^CLT:.*N/A'`" ]; then
    echo "You need to have XCode command line tools installed."
    echo "  $ xcode-select --install"
fi

ghc --version >& /dev/null
if [ $? ]; then
    true
else
    echo "Please install ghc."
    echo "  $ brew install ghc"
fi

cabal --version >& /dev/null
if [ $? ]; then
    true
else
    echo "Please install ghc."
    echo "  $ brew install cabal-install"
fi

# Make sure to have basic tools installed.
cabal update
cabal install happy alex
cabal install cpphs
cabal install gtk2hs-buildtools

# Homebrew stuff.
brew install zeromq
brew install libmagic

# XQuartz is required: http://xquartz.macosforge.org/landing/
# The easiest way is through Homebrew.
brew install Caskroom/cask/xquartz

brew install cairo
brew install pango

# For C compiler to pick up /usr/local and X11 stuff during Cabal builds.
# You may want to put this in your shell startup.
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig

./build.sh all
./build.sh display
