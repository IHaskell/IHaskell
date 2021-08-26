# should match the GHC version of the stack.yaml resolver
# checked in CI
ARG GHC_VERSION=8.10.4

FROM haskell:${GHC_VERSION} AS ihaskell_base

# Install Ubuntu packages needed for IHaskell runtime
RUN apt-get update && \
    apt-get install -y \
            libblas3 \
            libcairo2 \
            liblapack3 \
            libmagic1 \
            libpango-1.0-0 \
            libzmq5 \
        && \
    rm -rf /var/lib/apt/lists/*

FROM ihaskell_base AS builder

# Install Ubuntu packages needed for IHaskell build
RUN apt-get update && \
    apt-get install -y \
            libblas-dev \
            libcairo2-dev \
            liblapack-dev \
            libmagic-dev \
            libpango1.0-dev \
            libzmq3-dev \
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
RUN stack build --only-snapshot

# Build IHaskell itself.
# Don't just `COPY .` so that changes in e.g. README.md don't trigger rebuild.
COPY src src
COPY html html
COPY main main
COPY LICENSE LICENSE
RUN stack install --local-bin-path ./bin/

# Save resolver used to build IHaskell
RUN sed -n 's/resolver: \(.*\)/\1/p' stack.yaml | tee resolver.txt

# Save third-party data files
RUN mkdir /data && \
    snapshot_install_root=$(stack path --snapshot-install-root) && \
    cp $(find ${snapshot_install_root} -name hlint.yaml) /data

FROM ihaskell_base AS ihaskell

# Install Jupyter notebook
RUN apt-get update && \
    apt-get install -y python3-pip && \
    rm -rf /var/lib/apt/lists/*
RUN pip3 install -U jupyter

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

# Set up env file
RUN stack exec env --system-ghc > ${IHASKELL_DATA_DIR}/env

# Install + setup IHaskell
COPY --from=builder --chown=${NB_UID} /build/bin/ihaskell /usr/local/bin/
COPY --from=builder --chown=${NB_UID} /build/html ${IHASKELL_DATA_DIR}/html
RUN export ihaskell_datadir=${IHASKELL_DATA_DIR} && \
    ihaskell install --env-file ${IHASKELL_DATA_DIR}/env
RUN jupyter notebook --generate-config

CMD ["jupyter", "notebook", "--ip", "0.0.0.0"]
