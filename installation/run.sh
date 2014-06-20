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
# Only add IHASKELL_IPYTHON_ARGS to notebook.
if [[ $1 == "notebook" ]]; then
    ipython "$@" $IHASKELL_IPYTHON_ARGS
else
    ipython "$@"
fi
