#!/bin/bash

# Installation for Ubuntu Linux (tested on v14.10) from IHaskell repo directory.
# TODO Split out setup for installation from Hackage released versions.

# Install the dependencies as root user
sudo ./root-deps.sh

# Make sure to have basic tools installed.
cabal update
cabal install happy alex
cabal install cpphs
cabal install gtk2hs-buildtools
cabal install HTTP

# Build ihaskell, and all the display packages
./build.sh all
