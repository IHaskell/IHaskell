#!/bin/sh

# Installation for Linux (tested on Ubuntu 14.10) from IHaskell repo directory.
# TODO Split out setup for installation from Hackage released versions.

ghc --version >& /dev/null
if [ $? ]; then
    true
else
    echo "Please install ghc."
fi

cabal --version >& /dev/null
if [ $? ]; then
    true
else
    echo "Please install Cabal."
fi

# Install IPython.
# python-pip is out of date, causes problems, so we get the latest version
# using easy_install instead.
#sudo apt-get install python-pip
sudo apt-get install -y python-dev

easy_install -U pip
pip install -U 'ipython[all]'

# Make sure to have basic tools installed.
cabal update
cabal install happy alex
cabal install cpphs
cabal install gtk2hs-buildtools

# C libraries
sudo apt-get install -y libtinfo-dev
sudo apt-get install -y libzmq3-dev

sudo apt-get install -y libcairo2-dev
sudo apt-get install -y libpango1.0-dev

./build.sh all
./build.sh display
