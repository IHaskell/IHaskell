#!/bin/bash

# Installation for Linux (tested on Ubuntu 14.10) from IHaskell repo directory.
# TODO Split out setup for installation from Hackage released versions.

if [ -z "$(which ghc)" ]; then
    echo "Please install ghc."
    exit 1
fi

if [ -z "$(which cabal)" ]; then
    echo "Please install Cabal."
    exit 1
fi

# If ipython is installed, and has a major version >= 3, then use the
# installed version. Otherwise we install from scratch.
if [ -n "$(which ipython)" ] && { [ $(ipython --version | tr '.' ' ' | cut -f 1 -d ' ') -ge 3 ]; } then
    echo "Using ipython already installed:" "$(which ipython)"
else
    # Install IPython.
    # python-pip is out of date, causes problems, so we get the latest version
    # using easy_install instead.
    #sudo apt-get install python-pip
    sudo apt-get install -y python-dev

    # To get easy_install
    sudo apt-get install -y python-setuptools

    sudo easy_install -U pip

    # -H to use .cache in /root
    sudo -H pip install -U 'ipython[all]'
fi

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

sudo apt-get install -y libmagic-dev

sudo apt-get install -y libblas-dev
sudo apt-get install -y liblapack-dev

./build.sh all
