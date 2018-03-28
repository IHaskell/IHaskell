FROM fpco/stack-build:lts-11.2

# Install all necessary Ubuntu packages
RUN apt-get update && apt-get install -y python3-pip libgmp-dev libmagic-dev libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libblas-dev liblapack-dev gcc g++ && \
    rm -rf /var/lib/apt/lists/*

# Install Jupyter notebook
RUN pip3 install -U jupyter

ENV LANG en_US.UTF-8

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
ENV PATH $(stack path --local-install-root)/bin:$(stack path --snapshot-install-root)/bin:$(stack path --compiler-bin):/root/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin
RUN ihaskell install
ENTRYPOINT stack exec -- jupyter notebook --allow-root --NotebookApp.port=8888 '--NotebookApp.ip=*' --NotebookApp.notebook_dir=/notebooks
EXPOSE 8888
