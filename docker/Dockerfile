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

ARG BASE_CONTAINER=ubuntu:bionic-20180526@sha256:c8c275751219dadad8fa56b3ac41ca6cb22219ff117ca98fe82b42f24e1ba64e
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
##
## IHaskell.Display dependencies
#    libmagic-dev \
#    libcairo2-dev \
#    libblas-dev \
#    liblapack-dev \
#    g++ \
#
# Required for building ihaskell_labextension
    npm \
    && \
#
# Clean apt install
    rm -rf /var/lib/apt/lists/* \
    && \
#
# Install latest npm, which is not in Ubuntu repo
# Because ihaskell_labextension requires npm >= 4.0
    npm install npm@latest -g \
    && \
#
# Clean npm install cache
    npm cache clean --force \
    && true

ENV LANG     C.UTF-8
ENV LANGUAGE C.UTF-8
ENV LC_ALL   C.UTF-8
ENV LC_CTYPE C.UTF-8

EXPOSE 8888
