FROM ubuntu:14.04

# Install all necessary Ubuntu packages
RUN apt-get update && apt-get install -y python-dev python-setuptools libmagic-dev libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libblas-dev liblapack-dev gcc g++

# Install Jupyter notebook
RUN easy_install -U pip && pip install -U jupyter

# Install stack from the FPComplete repositories.
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442 && \
    echo 'deb http://download.fpcomplete.com/ubuntu trusty main' > /etc/apt/sources.list.d/fpco.list && \
    apt-get update && \
    apt-get install -y stack

# Set up a working directory for IHaskell
RUN mkdir /ihaskell
WORKDIR /ihaskell

# Set up stack
COPY stack.yaml stack.yaml
RUN stack setup

# Install dependencies for IHaskell
COPY ihaskell.cabal ihaskell.cabal
COPY ipython-kernel ipython-kernel
COPY ghc-parser ghc-parser
COPY ihaskell-display ihaskell-display
RUN stack build --only-snapshot

# Install IHaskell itself. Don't just COPY . so that
# changes in e.g. README.md don't trigger rebuild.
COPY src /ihaskell/src
COPY html /ihaskell/html
COPY main /ihaskell/main
COPY LICENSE /ihaskell/LICENSE
RUN stack build && stack install

# Run the notebook
RUN mkdir /notebooks
ENV PATH /ihaskell/.stack-work/install/x86_64-linux/nightly-2015-08-15/7.10.2/bin:/root/.stack/snapshots/x86_64-linux/nightly-2015-08-15/7.10.2/bin:/root/.stack/programs/x86_64-linux/ghc-7.10.2/bin:/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN ihaskell install
ENTRYPOINT stack exec -- jupyter notebook --NotebookApp.port=8888 '--NotebookApp.ip=*' --NotebookApp.notebook_dir=/notebooks
EXPOSE 8888
