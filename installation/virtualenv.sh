#!/bin/sh

# Which version of virtualenv to use.
VIRTUALENV=virtualenv-1.9

# Where to install the virtualenv.
DESTINATION=$1

# Download virtualenv.
curl -O https://pypi.python.org/packages/source/v/virtualenv/$VIRTUALENV.tar.gz
tar xvfz $VIRTUALENV.tar.gz
cd $VIRTUALENV

# Create a virtualenv.
python virtualenv.py $DESTINATION
