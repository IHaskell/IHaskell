# Install python's dependencies in a virtualenv
mkdir --parents ~/.ihaskell/ipython/
virtualenv ~/.ihaskell/ipython
source ~/.ihaskell/ipython/bin/activate
pip install -r ./requirements.txt
deactivate
