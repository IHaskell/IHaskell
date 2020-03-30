# This Dockerfile builds an image with all of the Ubuntu package requirements
# for building and running IHaskell. For discussion of how to use this
# image, see the Docker section of the `README.md`.
#
#     docker build -t ihaskell-dev .
#
# To build the image from a different Ubuntu base, specify a
# different BASE_CONTAINER.
#
#     docker build --build-arg BASE_CONTAINER=ubuntu:16.04 -t ihaskell-dev .
#

ARG BASE_CONTAINER=ubuntu:bionic-20200219@sha256:0925d086715714114c1988f7c947db94064fd385e171a63c07730f1fa014e6f9
FROM $BASE_CONTAINER

RUN apt-get update && apt-get install -y --no-install-recommends \
#
# Required for GHC
# https://docs.haskellstack.org/en/stable/docker_integration/#custom-images
    gcc \
    libgmp-dev \
#
# Python requirements for installing jupyter
    python3-pip \
    python3-setuptools \
    python3-dev \
    python3-wheel \
#
# ihaskell kernel dependencies
    libtinfo-dev \
    libzmq3-dev \
    libpango1.0-dev \
#
# IHaskell.Display dependencies
    libmagic-dev \
    libcairo2-dev \
    libblas-dev \
    liblapack-dev \
    g++ \
#
# Required for building ihaskell_labextension
    npm \
#
# Required for upgrading nodejs
    curl \
#
# Clean apt install
  && rm -rf /var/lib/apt/lists/* \
#
# Install latest npm, which is not in Ubuntu repo
# Because ihaskell_labextension requires npm >= 4.0
  && npm install npm@latest -g \
#
# Clean npm install cache
  && npm cache clean --force

# Setup nodejs repository
# https://github.com/nodesource/distributions/blob/master/README.md#installation-instructions
# Need nodejs later than 8.16.2 for jupyterlab 2.0.2 → rendermime → marked 0.8.0
# Otherwise when we build ihaskell_labextension we get errors like this:
#
#     ihaskell_labextension $ npm install
#     npm WARN notsup Unsupported engine for marked@0.8.2: wanted: {"node":">= 8.16.2"} (current: {"node":"8.10.0","npm":"6.14.4"})
#
RUN curl -sL https://deb.nodesource.com/setup_13.x | bash - \
  && apt-get install -y nodejs \
#
# Clean apt install
  && rm -rf /var/lib/apt/lists/*

ENV LANG     C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL   C.UTF-8
ENV LC_CTYPE C.UTF-8

EXPOSE 8888
