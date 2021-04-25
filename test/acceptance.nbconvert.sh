#! /usr/bin/env bash

# Run nbconvert acceptance tests
# ------------------------------
#
# This script must be called from the root directory of IHaskell. It requires
# `jq` to be available in the $PATH.
#
# Positional arguments to this script are the invocation for `nbconvert`.
# For example:
#
# Invoke from the the root IHaskell directory:
#
#    test/acceptance.nbconvert.sh jupyter nbconvert
#
# Invoke with `stack` from the the root IHaskell directory:
#
#    test/acceptance.nbconvert.sh stack exec -- jupyter nbconvert
#
# Invoke with Stack+Docker from the the root IHaskell directory:
#
#    test/acceptance.nbconvert.sh stack --docker exec -- jupyter nbconvert
#
# Invoke with Nix from the root IHaskell directory:
#
#    test/acceptance.nbconvert.sh result/bin/ihaskell-nbconvert
#

set -euo pipefail

$* --to=notebook --execute --allow-errors --stdout --nbformat=4 test/acceptance.nbconvert.in.ipynb > test/acceptance.nbconvert.out.ipynb

diff \
  <(grep -v -e 'version' -e 'Line ' -e 'Integral' -e 'Num'  \
    <(cat test/acceptance.nbconvert.in.ipynb | jq '{"cells": .cells | map(del(.metadata.execution)), "metadata": .metadata}')) \
  <(grep -v -e 'version' -e 'Line ' -e 'Integral' -e 'Num'  \
    <(cat test/acceptance.nbconvert.out.ipynb | jq '{"cells": .cells | map(del(.metadata.execution)), "metadata": .metadata}'))

