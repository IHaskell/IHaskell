#!/bin/bash
set -e

# Which version of virtualenv to use.
VIRTUALENV=virtualenv-1.9.1

# Where to install the virtualenv.
DESTINATION=$1

# Download virtualenv.
echo "Downloading virtualenv."
curl -O https://pypi.python.org/packages/source/v/virtualenv/$VIRTUALENV.tar.gz
tar xvfz $VIRTUALENV.tar.gz
cd $VIRTUALENV

# Create a virtualenv.
echo "Creating a virtualenv."
python virtualenv.py $DESTINATION
