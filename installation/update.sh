#!/bin/bash
set -e

# Which virtualenv to use.
VIRTUALENV=$1

# Commit hash to install.
COMMIT=$2

# Find out current IPython commit hash.
cd $VIRTUALENV/src/ipython
CURRENT_COMMIT=`git rev-parse HEAD`
if [ $CURRENT_COMMIT != $COMMIT ]; then
  # Activate the virtualenv.
  source $VIRTUALENV/bin/activate

  # Update IPython.
  echo "Updating IPython (this may take a while)."
  pip install --upgrade -e git+https://github.com/gibiansky/ipython.git@$COMMIT#egg=ipython-dev
fi;
