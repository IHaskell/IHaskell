#!/bin/bash
set -e

# Which virtualenv to use.
VIRTUALENV=$1
shift

# Activate the virtualenv, if it exists.
if [[ -f $VIRTUALENV/bin/activate ]]; then
  source $VIRTUALENV/bin/activate;
fi

# Run IPython.
# Quotes around $@ are necessary to deal properly with spaces.
ipython "$@" $IHASKELL_IPYTHON_ARGS
