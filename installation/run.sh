#!/bin/bash
set -e

# Which virtualenv to use.
VIRTUALENV=$1
shift

# Activate the virtualenv.
source $VIRTUALENV/bin/activate

# Run IPython.
# Quotes around $@ are necessary to deal properly with spaces.
ipython "$@" $IHASKELL_IPYTHON_ARGS
