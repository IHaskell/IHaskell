#!/bin/bash
set -e

# Which virtualenv to use.
VIRTUALENV=$1
shift

# Activate the virtualenv.
source $VIRTUALENV/bin/activate

# Run IPython.
ipython $@
