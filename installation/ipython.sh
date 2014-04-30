#!/bin/bash
set -e

# Which virtualenv to use.
VIRTUALENV=$1

# Activate the virtualenv.
source $VIRTUALENV/bin/activate

# Upgrade pip.
echo "Upgrading pip."
pip install --upgrade "pip>=1.4.1"

# Install all necessary dependencies with Pip.
echo "Installing dependency (pyzmq)."
pip install pyzmq==14.0.1

echo "Installing dependency (markupsafe)."
pip install markupsafe==0.18

echo "Installing dependency (jinja2)."
pip install jinja2==2.7.1

echo "Installing dependency (tornado)."
pip install tornado==3.1.1

echo "Installing dependency (pygments)."
pip install pygments==1.6

# Install IPython itself.
echo "Installing IPython (this may take a while)."
pip install ipython
