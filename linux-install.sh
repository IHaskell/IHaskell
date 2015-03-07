#!/bin/sh

# Installation for Linux (tested on Ubuntu 14.10) from IHaskell repo directory.
# TODO Split out setup for installation from Hackage released versions.

# Install IPython.
# python-pip is out of date, causes problems, so we get the latest version.
#sudo apt-get install python-pip
easy_install -U pip
pip install -U 'ipython[all]'

# Install GHC, Cabal, Alex, Happy

# Adjust these as you desire.
CABALVER=1.22
GHCVER=7.8.4
# You will want to add this to your shell startup to pick up
# what gets installed by Cabal.
export PATH=~/.cabal/bin:/opt/cabal/$CABALVER/bin:/opt/ghc/$GHCVER/bin:$PATH

sudo apt-get update
sudo apt-get install -y software-properties-common
sudo add-apt-repository -y ppa:hvr/ghc
sudo apt-get update
sudo apt-get install -y cabal-install-$CABALVER ghc-$GHCVER

cabal install alex happy

sudo apt-get install -y python-dev

sudo apt-get install -y libtinfo-dev
sudo apt-get install -y libzmq3-dev

sudo apt-get install -y libcairo2-dev
sudo apt-get install -y libpango1.0-dev

./build.sh all
./build.sh display
