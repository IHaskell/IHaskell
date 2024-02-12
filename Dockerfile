# should match the GHC version of the stack.yaml resolver
# checked in CI
ARG GHC_VERSION=9.6.4

FROM haskell:${GHC_VERSION} AS ihaskell_base

# Install Ubuntu packages needed for IHaskell runtime
RUN apt-get update && \
    apt-get install -y libzmq5 \
        && \
    rm -rf /var/lib/apt/lists/*

FROM ihaskell_base AS builder

# Install Ubuntu packages needed for IHaskell build
RUN apt-get update && \
    apt-get install -y libzmq3-dev pkg-config \
        && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /build

# Build snapshot
COPY stack.yaml stack.yaml
COPY ihaskell.cabal ihaskell.cabal
COPY ipython-kernel ipython-kernel
COPY ghc-parser ghc-parser
COPY ihaskell-display ihaskell-display
RUN stack setup
RUN stack build ihaskell --only-snapshot

# Build IHaskell itself.
# Don't just `COPY .` so that changes in e.g. README.md don't trigger rebuild.
COPY src src
COPY html html
COPY main main
COPY jupyterlab-ihaskell jupyterlab-ihaskell
COPY LICENSE LICENSE
RUN stack install ihaskell --local-bin-path ./bin/

# Save resolver used to build IHaskell
RUN sed -n 's/resolver: \(.*\)#.*/\1/p' stack.yaml | tee resolver.txt

# Save third-party data files
RUN mkdir /data && \
    snapshot_install_root=$(stack path --snapshot-install-root) && \
    cp $(find ${snapshot_install_root} -name hlint.yaml) /data

FROM ihaskell_base AS ihaskell

# Install JupyterLab
RUN apt-get update && \
    apt-get install -y python3-pip && \
    rm -rf /var/lib/apt/lists/*
RUN pip3 install -U pip
RUN pip3 install -U jupyterlab notebook

# Create runtime user
ENV NB_USER jovyan
ENV NB_UID 1000
RUN adduser --disabled-password \
    --gecos "Default user" \
    --uid ${NB_UID} \
    ${NB_USER}

# Create directory for storing ihaskell files
ENV IHASKELL_DATA_DIR /usr/local/lib/ihaskell
RUN mkdir -p ${IHASKELL_DATA_DIR} && chown ${NB_UID} ${IHASKELL_DATA_DIR}

# Set up + set hlint data directory
ENV HLINT_DATA_DIR /usr/local/lib/hlint
COPY --from=builder --chown=${NB_UID} /data/hlint.yaml ${HLINT_DATA_DIR}/
ENV hlint_datadir ${HLINT_DATA_DIR}

# Set current user + directory
WORKDIR /home/${NB_USER}/src
RUN chown -R ${NB_UID} /home/${NB_USER}/src
USER ${NB_UID}

# Set up global project
COPY --from=builder --chown=${NB_UID} /build/resolver.txt /tmp/
RUN stack setup --resolver=$(cat /tmp/resolver.txt) --system-ghc
RUN stack config set system-ghc --global true

# Set up env file
RUN stack exec env --system-ghc > ${IHASKELL_DATA_DIR}/env

# Install + setup IHaskell
COPY --from=builder --chown=${NB_UID} /build/bin/ihaskell /usr/local/bin/
COPY --from=builder --chown=${NB_UID} /build/html ${IHASKELL_DATA_DIR}/html
COPY --from=builder --chown=${NB_UID} /build/jupyterlab-ihaskell ${IHASKELL_DATA_DIR}/jupyterlab-ihaskell
RUN export ihaskell_datadir=${IHASKELL_DATA_DIR} && \
    ihaskell install --env-file ${IHASKELL_DATA_DIR}/env
RUN jupyter notebook --generate-config

CMD ["jupyter", "notebook", "--ip", "0.0.0.0"]
