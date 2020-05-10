FROM ubuntu:18.04

ARG STACK_VERSION=2.3.1
ARG RESOLVER=lts-14.27

# Install all necessary Ubuntu packages
RUN apt-get update && apt-get install -y python3-pip libgmp-dev libmagic-dev libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libblas-dev liblapack-dev gcc g++ wget && \
    rm -rf /var/lib/apt/lists/*

# Install Jupyter notebook
RUN pip3 install -U jupyter

ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8
ENV NB_USER jovyan
ENV NB_UID 1000
ENV HOME /home/${NB_USER}

RUN adduser --disabled-password \
    --gecos "Default user" \
    --uid ${NB_UID} \
    ${NB_USER}

RUN wget -qO- https://github.com/commercialhaskell/stack/releases/download/v$STACK_VERSION/stack-$STACK_VERSION-linux-x86_64.tar.gz | tar xz --wildcards --strip-components=1 -C /usr/bin '*/stack'

# Set up a working directory for IHaskell
RUN install -d -o ${NB_UID} -g ${NB_UID} ${HOME} ${HOME}/ihaskell
WORKDIR ${HOME}/ihaskell

USER ${NB_UID}

# Install dependencies for IHaskell
COPY --chown=${NB_UID}:${NB_UID} stack.yaml stack.yaml
COPY --chown=${NB_UID}:${NB_UID} ihaskell.cabal ihaskell.cabal
COPY --chown=${NB_UID}:${NB_UID} ipython-kernel ipython-kernel
COPY --chown=${NB_UID}:${NB_UID} ghc-parser ghc-parser
COPY --chown=${NB_UID}:${NB_UID} ihaskell-display ihaskell-display

RUN stack setup
RUN stack build --only-snapshot

# Install IHaskell itself. Don't just COPY . so that
# changes in e.g. README.md don't trigger rebuild.
COPY --chown=${NB_UID}:${NB_UID} src ${HOME}/ihaskell/src
COPY --chown=${NB_UID}:${NB_UID} html ${HOME}/ihaskell/html
COPY --chown=${NB_UID}:${NB_UID} main ${HOME}/ihaskell/main
COPY --chown=${NB_UID}:${NB_UID} LICENSE ${HOME}/ihaskell/LICENSE

RUN stack build && stack install
RUN mkdir -p ${HOME}/.stack/global-project && \
    echo "packages: []\nresolver: ${RESOLVER}\n" > ${HOME}/.stack/global-project/stack.yaml

# Run the notebook
ENV PATH $(stack path --local-install-root)/bin:$(stack path --snapshot-install-root)/bin:$(stack path --compiler-bin):/home/${NB_USER}/.local/bin:${PATH}
RUN ihaskell install --stack
WORKDIR ${HOME}
RUN jupyter notebook --generate-config
CMD ["jupyter", "notebook", "--ip", "0.0.0.0"]
