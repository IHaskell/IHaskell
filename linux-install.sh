#!/bin/sh

# Installation for Linux (tested on Ubuntu 14.10)

sudo apt-get install python-dev
sudo apt-get install python-pip
sudo pip install -U 'ipython[all]'

sudo apt-get install libtinfo-dev
sudo apt-get install libzmq3-dev

sudo apt-get install libcairo2-dev
sudo apt-get install libpango1.0-dev

./build.sh all
./build.sh display
