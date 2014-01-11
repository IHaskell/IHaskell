#! /bin/bash
source ~/.ihaskell/ipython/bin/activate
pip install --install-option="--prefix=~/.ihaskell/ipython" -e git+https://github.com/ipython/ipython.git@ae1f2befd1372f37562420aab9d87a0a625ba58d#egg=ipython-dev
deactivate