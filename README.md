![jupyter](https://i.imgur.com/S16l2Hw.png) ![IHaskell](https://i.imgur.com/qhXXFbA.png) [![Build Status](https://travis-ci.org/gibiansky/IHaskell.svg?branch=master)](https://travis-ci.org/gibiansky/IHaskell)

# IHaskell

> You can now try IHaskell directly in your browser at [try.jupyter.org](https://try.jupyter.org).
>
> Alternatively, watch a [talk and demo](http://begriffs.com/posts/2016-01-20-ihaskell-notebook.html) showing off IHaskell features.

IHaskell is a kernel for the [Jupyter project](http://ipython.org), which allows you to use Haskell inside Jupyter frontends (including the console and notebook). It currently supports GHC 8 and 8.2. For GHC 7.10 support please use the [`GHC7`](https://github.com/gibiansky/IHaskell/releases/tag/GHC7) tag.

For a tour of some IHaskell features, check out the [demo Notebook](http://nbviewer.ipython.org/github/gibiansky/IHaskell/blob/master/notebooks/IHaskell.ipynb). More example notebooks are available on the [wiki](https://github.com/gibiansky/IHaskell/wiki).
The [wiki](https://github.com/gibiansky/IHaskell/wiki) also has more extensive documentation of IHaskell features.

![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)
![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)

### Interactive In-Browser Notebook


**Note:** IHaskell does not support Windows. To use on Windows, install
Virtualbox, install Ubuntu or another Linux distribution, and proceed with the
install instructions.

# Installation

## Linux

Some prerequisites; adapt to your distribution.

```bash
sudo apt-get install -y python3-pip git libtinfo-dev libzmq3-dev libcairo2-dev libpango1.0-dev libmagic-dev libblas-dev liblapack-dev
```

```bash
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
stack install gtk2hs-buildtools
stack install --fast
ihaskell install --stack
```

## Mac

You need to have [Homebrew](https://brew.sh) installed. 
If you do not have it yet run `/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"` in your terminal.

You also need the Xcode command line tools.
You can install them by running `xcode-select --install` in the terminal and following the prompts.

```bash
brew install python3 zeromq libmagic cairo pkg-config haskell-stack pango
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
stack install gtk2hs-buildtools
stack install --fast
ihaskell install --stack
```

_Tested on macOS Sierra (10.12.6)_

# Running

```bash
stack exec jupyter -- notebook
```

## Docker

If you prefer a Docker-based workflow, you can use it to create an IHaskell
notebook environment. For example:

```bash
$ docker build -t ihaskell:latest .
$ docker run -it -p8888:8888 ihaskell:latest
```

Currently the component that takes the longest time to compile is
`ihaskell-widgets`, so if you're in a hurry you may want to comment that out in
`stack.yaml`.

## Stack development with Docker
This is an alternative way to use Docker than above, taking advantage of stack's Docker support.
Makes it easy to install new packages with `stack --docker install <pkg>` without having to rebuild a docker image.
Like the other Docker workflow, this doesn't require any host dependecies to be installed.

```bash
docker build -t ihaskell-dev docker
stack --docker setup
stack --docker install
stack --docker exec ihaskell -- install --stack
stack --docker exec jupyter -- notebook --ip=0.0.0.0 notebooks
```

Everything in the LTS can be made available!
To add a package outside the LTS, simply add it to the `stack.yaml` file  (See: "Where are my packages?" below).
Then install the package with stack before restarting `jupyter`

```bash
# after adding details about mypackage to stack.yaml
stack --docker install mypackage
stack --docker exec jupyter -- notebook notebooks
```

## Nix

If you have the `nix` package manager installed, you can create an IHaskell
notebook environment with one command. For example:

```bash
$ nix-build release.nix --arg packages "haskellPackages: [ haskellPackages.lens ]"
<result path>
$ <result path>/bin/ihaskell-notebook
```

It might take a while the first time, but subsequent builds will be much faster.

# Troubleshooting

## Where are my packages? (IHaskell + Stack)

Stack manages separate environments for every package. By default your notebooks
will only have access to a few packages that happen to be required for
ihaskell. To make packages available add them to the stack.yaml in the ihaskell
directory and run `stack solver && stack install`.

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
