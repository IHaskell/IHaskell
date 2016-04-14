#!/bin/sh
set -e

# Installation for Mac OS X from IHaskell repo directory.
# TODO Split out setup for installation from Hackage released versions.
#
# This script assumes use of Homebrew.
# It is assumed you already have GHC and Cabal installed through Homebrew
# and your environment set up to install executables through Cabal.
#
# Also, XCode command line tools must be installed.
#
# XQuartz is needed for Cairo and Pango.

function abort() {
    for line
    do
        echo >&2 "$line"
    done
    exit 1
}

brew --version >& /dev/null || abort  \
    "Homebrew needs to be installed." \
    "  Download from http://brew.sh/"

# Install IPython.
if command -v pip3 >/dev/null 2>&1
then
    PIP=pip3
elif command -v pip >/dev/null 2>&1
then
    PIP=pip
else
    abort \
        "Python pip needs to be installed." \
        "  One way is to install Homebrew Python:" \
        "  $ brew install python"
fi

$PIP --version >& /dev/null || abort \
    "Python $PIP needs to be installed." \
    "  One way is to install Homebrew Python:" \
    "  $ brew install python"

$PIP install -U 'ipython[all]'

[ -n "`brew --config | grep '^CLT:.*N/A'`" ] && abort \
    "You need to have XCode command line tools installed." \
    "  $ xcode-select --install"

ghc --version >& /dev/null || abort \
    "Please install ghc." \
    "  $ brew install ghc"

cabal --version >& /dev/null || abort \
    "Please install Cabal." \
    "  $ brew install cabal-install"

# Make sure to have basic tools installed.
cabal update
cabal install happy alex
cabal install cpphs
cabal install gtk2hs-buildtools

# Homebrew stuff.
brew update
brew ls --versions zeromq    | grep -q . || brew install zeromq
brew ls --versions libmagic  | grep -q . || brew install libmagic

# XQuartz is required: http://xquartz.macosforge.org/landing/
# The easiest way is through Homebrew.
brew tap Caskroom/cask
brew      ls --versions brew-cask  | grep -q . || brew install brew-cask
brew cask ls --versions xquartz    | grep -q . || brew cask install xquartz
brew      ls --versions cairo      | grep -q . || brew install cairo
brew      ls --versions pango      | grep -q . || brew install pango

# make cabal install magic, which won't work correctly if done using
# default flags, since Homebrew dumps libmagic into /usr/local/lib rather than /lib
brew_prefix=$(brew --prefix)
cabal install magic-1.1  --extra-lib "$brew_prefix/lib" --extra-include "$brew_prefix/include"

# For C compiler to pick up /usr/local and X11 stuff during Cabal builds.
# You may want to put this in your shell startup.
export PKG_CONFIG_PATH=/usr/local/lib/pkgconfig:/opt/X11/lib/pkgconfig

./build.sh all
