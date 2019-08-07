![jupyter](https://i.imgur.com/S16l2Hw.png) ![IHaskell](https://i.imgur.com/qhXXFbA.png) [![Build Status](https://travis-ci.org/gibiansky/IHaskell.svg?branch=master)](https://travis-ci.org/gibiansky/IHaskell) [![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/gibiansky/IHaskell/master)

# IHaskell

> You can now try IHaskell directly in your browser at [CoCalc](https://cocalc.com) or [mybinder.org](https://mybinder.org/v2/gh/gibiansky/IHaskell/master).
>
> Alternatively, watch a [talk and demo](http://begriffs.com/posts/2016-01-20-ihaskell-notebook.html) showing off IHaskell features.

IHaskell is a kernel for the [Jupyter project](https://jupyter.org), which allows you to use Haskell inside Jupyter frontends (including the console and notebook). It currently supports GHC 8, 8.2, 8.4, and 8.6. For GHC 7.10 support please use the [`GHC7`](https://github.com/gibiansky/IHaskell/releases/tag/GHC7) tag.

For a tour of some IHaskell features, check out the [demo Notebook](http://nbviewer.ipython.org/github/gibiansky/IHaskell/blob/master/notebooks/IHaskell.ipynb). More example notebooks are available on the [wiki](https://github.com/gibiansky/IHaskell/wiki).
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

```bash
curl -sSL https://get.haskellstack.org/ | sh
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
# stack install gtk2hs-buildtools # Disabled for now because gtk2hs-buildtools doesn't work with lts-13 yet
stack install --fast
ihaskell install --stack
```

If you want to use jupyterlab (right now only version ~0.33), you need to
install the jupyterlab ihaskell extension to get syntax highlighting with:

```bash
jupyter labextension install ihaskell_jupyterlab
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

```bash
brew install python3 zeromq libmagic cairo pkg-config haskell-stack pango
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
pip3 install -r requirements.txt
# stack install gtk2hs-buildtools # Disabled for now because gtk2hs-buildtools doesn't work with lts-13 yet
stack install --fast
ihaskell install --stack
```

If you have Homebrew installed to a custom location, you'd need to specify `--extra-include-dirs ${HOMEBREW_PREFIX}/include --extra-lib-dir ${HOMEBREW_PREFIX}/lib` to the `stack` command.

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
docker run --rm -it -p8888:8888 ihaskell:latest
```

## Stack and Docker

IHaskell, being a Jupyter kernel, depends at runtime on a tall pile of software
provided by, traditionally, `apt`, `pip`, and `npm`.
To develop IHaskell, we want to be able to isolate and control all of the
dependencies. We can use
[Stack's Docker integration](https://docs.haskellstack.org/en/stable/docker_integration/)
to install all of those runtime dependencies into an isolated environment.

* The system library dependencies installed with `apt` will be isolated
  in the `ihaskell-dev` Docker image.
* Dependencies installed by `pip` and `npm` will be isolated in
  the `IHaskell/.stack-work` subdirectory.
* All Stack build products and installed binaries will be isolated in the
  `IHaskell/.stack-work` subdirectory.

The following `stack --docker` commands require a Docker image
named `ihaskell-dev`, so build that image from the `docker/Dockerfile` with this
command:

```bash
docker build -t ihaskell-dev docker
```

Install the `ghc` version specified by the Stack `resolver`.

```bash
stack --docker setup
```

Install Jupyter and all of its requirements.
```bash
stack --docker exec pip3 -- install jupyter
```

Build IHaskell and all of its packages.

```bash
stack --docker install
```

Direct IHaskell to register itself as a Jupyter kernel.

```bash
stack --docker exec ihaskell -- install --stack
```

Optionally, install JupyterLab and the IHaskell JupyterLab extension for
syntax highlighting. See the
[`ihaskell_labextension/README.md`](ihaskell_labextension/README.md).

```bash
stack --docker exec pip3 -- install jupyterlab
stack --docker exec bash -- -c 'cd ihaskell_labextension;npm install;npm run build;jupyter labextension link .'
```

Run the Jupyter notebook, with security disabled for testing.

```bash
stack --docker exec jupyter -- notebook --NotebookApp.token='' notebooks
```

Run JupyterLab (if you installed it), with security disabled for testing.
```bash
stack --docker exec jupyter -- lab --NotebookApp.token='' notebooks
```
Everything in Stackage can be installed by `stack --docker install`.

To install a local package, add it to the `stack.yaml`
file  (See: "Where are my packages?" below).
Install the package with `stack`, then restart `jupyter`.

```bash
# after adding details about mypackage to stack.yaml
stack --docker install mypackage
```

To cleanly delete the entire Stack Docker development environment:

```bash
docker image rm ihaskell-dev
stack clean --full
```

## Nix

If you have the `nix` package manager installed, you can create an IHaskell
notebook environment with one command. For example:

```bash
$ nix-build -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz --argstr compiler ghc864 --arg packages "haskellPackages: [ haskellPackages.lens ]"
<result path>
$ <result path>/bin/ihaskell-notebook
```

It might take a while the first time, but subsequent builds will be much faster.

The IHaskell display modules are not loaded by default and have to be specified as additional packages:

```bash
$ NIXPKGS_ALLOW_BROKEN=1 nix-build -I nixpkgs=https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.03.tar.gz --argstr compiler ghc844 --arg packages "haskellPackages: [ haskellPackages.ihaskell-blaze haskellPackages.ihaskell-charts ]"
```

We use GHC 8.4 here because not all dependencies have been updated to support GHC 8.6 yet.

For more examples of using IHaskell with Nix, see https://github.com/vaibhavsagar/notebooks.

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

## openFile: does not exist (Stack + Docker)

If you try to run a notebook with `stack --docker` and see an IHaskell kernel
error that looks like this:

```
ihaskell: /opt/ghc/8.6.5/lib/ghc-8.6.5/settings: openFile: does not exist
```

Then delete your `~/.stack` directory and start over.
