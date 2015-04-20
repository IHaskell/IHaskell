#!/bin/bash

# Script for installing ihaskell dependencies. Specifically, this script
# installs dependencies using apt-get, and needs to be executed as root.

# Directly utilized for providing ihaskell to jupyter/docker-demo-images

if [ $(id -u) -ne 0 ]; then
    echo "Please run as root"
    exit 1
fi

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
if [ -n "$(which ipython)" ] && { [ $(ipython --version | cut -f 1 -d '.') -ge 3 ]; } then
    echo "Using ipython already installed: $(which ipython)"
else
    # Install IPython.
    # python-pip is out of date, causes problems, so we get the latest version
    # using easy_install instead.
    #sudo apt-get install python-pip
    apt-get install -y python-dev

    # To get easy_install
    apt-get install -y python-setuptools

    easy_install -U pip

    # -H to use .cache in /root
    pip install -U 'ipython[all]'
fi

# C libraries
apt-get install -y libtinfo-dev
apt-get install -y libzmq3-dev

apt-get install -y libcairo2-dev
apt-get install -y libpango1.0-dev

apt-get install -y libmagic-dev

apt-get install -y libblas-dev
apt-get install -y liblapack-dev
