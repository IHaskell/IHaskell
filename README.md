![jupyter](https://i.imgur.com/S16l2Hw.png) ![IHaskell](https://i.imgur.com/qhXXFbA.png) [![Build Status](https://github.com/gibiansky/IHaskell/workflows/CI/badge.svg)](https://github.com/gibiansky/IHaskell/actions?query=workflow%3ACI) [![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/gibiansky/IHaskell/mybinder)

# IHaskell

> You can now try IHaskell directly in your browser at [CoCalc](https://cocalc.com) or [mybinder.org](https://mybinder.org/v2/gh/gibiansky/IHaskell/mybinder).
>
> Alternatively, watch a [talk and demo](http://begriffs.com/posts/2016-01-20-ihaskell-notebook.html) showing off IHaskell features.

IHaskell is a kernel for the [Jupyter project](https://jupyter.org), which allows you to use Haskell inside Jupyter frontends (including the console and notebook). It currently supports GHC 8.0 through 9.0. For GHC 7.10 support please use the [`GHC7`](https://github.com/gibiansky/IHaskell/releases/tag/GHC7) tag.

For a tour of some IHaskell features, check out the [demo Notebook](http://nbviewer.org/github/gibiansky/IHaskell/blob/master/notebooks/IHaskell.ipynb). More example notebooks are available on the [wiki](https://github.com/gibiansky/IHaskell/wiki).
The [wiki](https://github.com/gibiansky/IHaskell/wiki) also has more extensive documentation of IHaskell features.

![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)
![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)

### Interactive In-Browser Notebook

# Installation

## Linux

Some prerequisites; adapt to your distribution.

```bash
sudo apt-get install -y python3-pip git libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libmagic-dev libblas-dev liblapack-dev
```

Install `stack`, clone this repository, install Python requirements, install
`ihaskell`, and install the Jupyter kernelspec with `ihaskell`.

These instructions assume you don't already have Stack or a Jupyter
installation, please skip the relevant steps if this is not the case.

```bash
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
stack install --fast
ihaskell install --stack
```

Run Jupyter.

```bash
stack exec jupyter -- notebook
```

## Mac

You need to have [Homebrew](https://brew.sh) installed.
If you do not have it yet run `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"` in your terminal.

You also need the Xcode command line tools.
You can install them by running `xcode-select --install` in the terminal and following the prompts.

These instructions assume you don't already have Stack or a Jupyter
installation, please skip the relevant steps if this is not the case.

```bash
brew install python3 zeromq libmagic cairo pkg-config haskell-stack pango
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
stack install --fast
ihaskell install --stack
```

If you have Homebrew installed to a location that `stack` does not expect (e.g. `/opt/homebrew`), you'd need to specify `--extra-include-dirs ${HOMEBREW_PREFIX}/include --extra-lib-dirs ${HOMEBREW_PREFIX}/lib` to the `stack` command.

Run Jupyter.

```bash
stack exec jupyter -- notebook
```

_Tested on macOS Sierra (10.12.6)_

## Windows

IHaskell does not support Windows, however it can be used on Windows 10 via
Windows Subsystem for Linux (WSL). If WSL is not installed, follow the
[Installation Guide for Windows 10](https://docs.microsoft.com/en-us/windows/wsl/install-win10).
The following assumes that Ubuntu is picked as the Linux distribution.

In the Ubuntu app, follow the steps above for Linux.

Jupyter Notebook is now ready to use. In the Ubuntu app, launch a Notebook
Server, without opening the notebook in a browser:

```bash
jupyter notebook --no-browser
```

Returning to Windows 10, open a browser and copy and paste the URL output in the
step above (the token will differ).

```bash
Or copy and paste one of these URLs:
     http://localhost:8888/?token=9ca8a725ddb1fdded176d9e0e675ba557ebb5fbef6c65fdf
```

_Tested on Windows 10 (build 18362.175) with Ubuntu 18.04 on WSL_

Alternatively, install Virtualbox, install Ubuntu or another Linux distribution,
and proceed with the install instructions.

## Docker

To quickly run a Jupyter notebook with the IHaskell kernel, try the `Dockerfile`
in the top directory.

```bash
docker build -t ihaskell:latest .
docker run --rm -p 8888:8888 ihaskell:latest
```

Or use the continuously updated Docker image 
[on Docker Hub](https://hub.docker.com/r/gibiansky/ihaskell).

```sh
docker run --rm -p 8888:8888 gibiansky/ihaskell
```

In order to mount your own local files into the Docker container
use following command:

```sh
docker run --rm -p 8888:8888 -v "$PWD":/home/jovyan/src gibiansky/ihaskell
```

Be aware that the directory you're mounting must contain
a `stack.yaml` file.
A simple version would be:

```yaml
resolver: lts-16.23
packages: []
```

It's recommended to use the same LTS version as the IHaskell image is using itself 
(as can be seen in [its stack.yaml](./stack.yaml)).
This guarantees that stack doesn't have to first perform 
a lengthy installation of GHC before running your notebook.

You can also use the following script to run IHaskell in Docker: https://gist.github.com/brandonchinn178/928d6137bfd17961b9584a8f96c18827

## Nix

If you have the `nix` package manager installed, you can create an IHaskell
notebook environment with one command. For example:

```bash
$ nix-build -I nixpkgs=https://github.com/NixOS/nixpkgs/tarball/nixos-23.05 release.nix --argstr compiler ghc928 --arg packages "haskellPackages: [ haskellPackages.lens ]"
<result path>
$ <result path>/bin/jupyter notebook
```

It might take a while the first time, but subsequent builds will be much
faster. You can use the
[https://ihaskell.cachix.org](https://app.cachix.org/cache/ihaskell) cache for
prebuilt artifacts.

The IHaskell display modules are not loaded by default and have to be specified as additional packages:

```bash
$ nix-build -I nixpkgs=https://github.com/NixOS/nixpkgs/tarball/nixos-23.05 release.nix --argstr compiler ghc928 --arg packages "haskellPackages: [ haskellPackages.ihaskell-blaze haskellPackages.ihaskell-charts ]"
```

For more examples of using IHaskell with Nix, see https://github.com/vaibhavsagar/notebooks.

# Developing

IHaskell is regularly updated to work with the latest version of GHC. To read how this is done, and how the development environment is set up, please see [this blog post](https://vaibhavsagar.com/blog/2021/05/02/updating-ihaskell-newer-ghc).

## Nix flake

There is also a Nix flake that provides a developer environment. For details on Nix flakes, please see the documentation at https://nixos.wiki/wiki/Flakes.

After this, IHaskell can be compiled as follows:

```bash
nix develop # This opens a new shell with all dependencies installed
cabal update # Make sure Cabal's package index is up-to-date
cabal build # Builds IHaskell
```

Note that this shell also provides `haskell-language-server`, which can be used in your editor if it supports it. Opening your editor from within the `nix develop` shell should allow it to see `haskell-language-server`.

# Troubleshooting

## Where are my packages? (IHaskell + Stack)

Stack manages separate environments for every package. By default your notebooks
will only have access to a few packages that happen to be required for
IHaskell. To make packages available add them to the stack.yaml in the IHaskell
directory and run `stack install --fast`.

Packages should be added to the `packages:` section and can take the following
form
([reproduced here from the stack documentation](https://github.com/commercialhaskell/stack/blob/master/doc/yaml_configuration.md#packages)). If
you've already installed a package by `stack install` you can simply list its
name even if it's local.

```
- package-name
- location: .
- location: dir1/dir2
- location: https://example.com/foo/bar/baz-0.0.2.tar.gz
- location: http://github.com/yesodweb/wai/archive/2f8a8e1b771829f4a8a77c0111352ce45a14c30f.zip
- location:
    git: git@github.com:commercialhaskell/stack.git
    commit: 6a86ee32e5b869a877151f74064572225e1a0398
- location:
    hg: https://example.com/hg/repo
    commit: da39a3ee5e6b4b0d3255bfef95601890afd80709
```

## The kernel keeps dying (IHaskell + Stack)

The default instructions globally install IHaskell with support for only one
version of GHC. If you've e.g. installed an `lts-10` IHaskell and are using it
with an `lts-9` project the mismatch between GHC 8.2 and GHC 8.0 will cause
this error. Stack also has the notion of a 'global project' located at
`~/.stack/global-project/` and the `stack.yaml` for that project should be on
the same LTS as the version of IHaskell installed to avoid this issue.
