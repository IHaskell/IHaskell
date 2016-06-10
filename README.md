IHaskell is a kernel for the [Jupyter project](http://ipython.org), which allows you to use Haskell inside Jupyter frontends, including the console and in-browser notebook.

### Status

[![Join the chat at https://gitter.im/gibiansky/IHaskell](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/gibiansky/IHaskell?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
[![Build Status](https://travis-ci.org/gibiansky/IHaskell.svg?branch=master)](https://travis-ci.org/gibiansky/IHaskell)

![IHaskell](https://raw.github.com/gibiansky/IHaskell/master/html/logo-64x64.png)

# IHaskell

> You can now try IHaskell directly in your browser at [try.jupyter.org](https://try.jupyter.org).
>
> Alternatively, watch a [talk and demo](http://begriffs.com/posts/2016-01-20-ihaskell-notebook.html) showing off IHaskell features.

IHaskell is a kernel for the [Jupyter project](http://ipython.org), which allows you to use Haskell inside Jupyter frontends (including the console and notebook).

For a tour of some IHaskell features, check out the [demo Notebook](http://nbviewer.ipython.org/github/gibiansky/IHaskell/blob/master/notebooks/IHaskell.ipynb). More example notebooks are available on the [wiki](https://github.com/gibiansky/IHaskell/wiki).
The [wiki](https://github.com/gibiansky/IHaskell/wiki) also has more extensive documentation of IHaskell features.

### Shell Usage
![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)

### Interactive In-Browser Notebook
![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)


# Source Installation

**Note:** IHaskell does not support Windows. To use on Windows, install
Virtualbox, install Ubuntu or another Linux distribution, and proceed with the
install instructions.

**How to get help:** Feel free to open an issue [on Github](https://github.com/gibiansky/IHaskell/issues?direction=desc&sort=updated&state=open) or join the [Gitter channel](https://gitter.im/gibiansky/IHaskell?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge).

Arch Linux has a package for IHaskell: https://aur.archlinux.org/packages/ihaskell-git/

Here is a blog post with step-by-step instructions for Ubuntu 14.04 (but should also work on other versions): https://remusao.github.io/install-ihaskell-on-ubuntu-1404-with-stack.html

### Docker Installation

The easiest way to use IHaskell is to install it inside a Docker container, which will come with the entire necessary stack, including Jupyter notebook. To install Docker, follow the [OS-specific instructions for your OS](https://docs.docker.com/engine/installation/).

To get the Docker image, pull it from the Docker Hub:
```bash
docker pull gibiansky/ihaskell:latest
```

You can then run IHaskell with:
```bash
docker run -it --volume $(pwd):/notebooks --publish 8888:8888 gibiansky/ihaskell:latest
```

If you wish to expose the Jupyter notebook on a port other than 8888, use the options `--publish 8888:$PORT` for any `PORT`.

If you'd like to build the image yourself, there is a provided `Dockerfile`, which you can build using:

```bash
# Build it in the repository directory
cd IHaskell/

# Building the image from scratch may take quite a while! (an hour or more)
docker build -t ihaskell:latest .

# Run the image without the gibiansky/ prefix
PORT=8888
docker run -it --volume $(pwd):/notebooks --publish 8888:$PORT ihaskell:latest
```

Open `localhost:$PORT` (`localhost:8888`) in your web browser to use IHaskell. If you are running on Mac OS X, then you will likely need to account for `docker-machine` and use the local IP of the machine instead of `localhost`.

### Install Using Installation Scripts

#### Ubuntu:

**If you are a user, and not a developer, it is recommended you use the `docker` instructions above instead of the Ubuntu installation script.**

If you are using a modern version of Ubuntu, clone the repository and then run the `ubuntu-install.sh` script:
```bash
git clone http://www.github.com/gibiansky/IHaskell
cd IHaskell
./ubuntu-install.sh
```
This script will ask you for `sudo` permissions in order to install IHaskell dependencies. The script is readable and easy to inspect if you wish to know what it does before giving it root permissions.
#### Mac OS X:

On Mac OS X, clone the repository and then run the `macos-install.sh` script:
```bash
git clone http://www.github.com/gibiansky/IHaskell
cd IHaskell
./macos-install.sh
```
Note that you must have [Homebrew](http://brew.sh/) installed for this script to work.

### Installing Manually

#### Install IPython
Install IPython 3.0 or above:
```bash
pip install ipython[all]
```
This may require root permissions on some systems, in which case put a `sudo` before that command before running it.
Once this is done, running `ipython --version` should print out `3.0` or above.

Note that IHaskell *requires* 3.0 or above; IHaskell *will not work* with IPython 2 or earlier.

#### Install Haskell

You can let [Stack](http://www.stackage.org/) take care of everything by running `stack setup` from within the IHaskell folder. Stack can also be used to build IHaskell later and will manage dependencies better than cabal (like in issue #578).

Or you can install GHC and Cabal manually. You must have appropriate versions of both:
```bash
ghc --numeric-version # Should be 7.6.* or 7.8.* or 7.10.*
cabal --version       # Should be 1.18.* or newer
```
GHC and Cabal may be installed in a number of other ways, including the [Haskell Platform](http://www.haskell.org/platform/), as a [standalone Mac app](https://github.com/ghcformacosx/ghc-dot-app), via Homebrew with `brew install ghc cabal-install`, and so on.


#### Install ZeroMQ
Install ZeroMQ, a library IHaskell uses for asynchronous communication.

  - **Mac OS X**: 
    - With [Homebrew](http://brew.sh/) installed, run `brew install zeromq`. (If using 32-bit Haskell Platform, you *may* need to use `brew install zeromq --universal`. YMMV.)
    - With [MacPorts](https://www.macports.org/) installed, run `ports install zmq`
  - **Ubuntu**: Run `sudo apt-get install libzmq3-dev`.
  - **Other**: You can install ZeroMQ from source or use another package manager:
```bash
# Compiling from source:
git clone git@github.com:zeromq/zeromq4-x.git libzmq
cd libzmq
./autogen.sh && ./configure && make
sudo make install
sudo ldconfig
```
If your own platform has a package and I haven't included instructions for it, feel free to send me an email or a PR on this README.

#### Install Haskell Tools

*(This section can be skipped when using stack)*

First, make sure that executables installed by `cabal` are on your shell `PATH`:
```bash
# If you have a ~/.cabal/bin folder:
export PATH=~/.cabal/bin:$PATH

# If you have a ~/Library/Haskell/bin folder on OS X:
export PATH=~/Library/Haskell/bin:$PATH
```

Then, install the `happy` parser generator tool and `cpphs` preprocessor:
```bash
cabal install happy cpphs
```

#### Build IHaskell
Install IHaskell! You may install it from Stackage via `stack install` (check the latest version on [http://www.stackage.org/lts]:
```bash
stack install ihaskell
```

Or you may install it from Hackage via `cabal install`:
```bash
cabal install ihaskell --reorder-goals
```
As IHaskell updates frequently, you may also want to clone the repository and install from there:
```bash
git clone http://www.github.com/gibiansky/IHaskell
cd IHaskell
./build.sh ihaskell # Build and install IHaskell
```

The build script, `build.sh`, is a script for building IHaskell and dependencies. It has the following modes:
- `ihaskell`: Build and install `ihaskell` and the two dependencies from this repository, `ipython-kernel` and `ghc-parser`.
- `quick`: Just install `ihaskell`, do not bother recompiling and reinstalling its dependencies (`ipython-kernel` and `ghc-parser`).
- `display`: Install `ihaskell` and all the support libraries in `ihaskell-display/`.
- `all`: Install everything, including `ihaskell`, the dependencies, and all the support libraries in `ihaskell-display/`.
It is run via `./build.sh all` or equivalent.

IHaskell may also be built in a sandbox, via something like:
```bash
cd IHaskell
cabal sandbox init
cabal sandbox add-source ihaskell-display/* ghc-parser ipython-kernel
cabal install . ihaskell-display/*
```

You may also need to use `--extra-lib-dirs` and `--extra-include-dirs`, if
`cabal` cannot find relevant libraries. For example:
```bash
cabal install . ihaskell-display/* --extra-lib-dirs=`brew --prefix libmagic`/lib --extra-include-dirs=`brew --prefix libmagic`/include
```

You can also build IHaskell with [stack](https://github.com/commercialhaskell/stack) instead of cabal:
```bash
cd IHaskell
stack install
```
The above will install `ihaskell`, all support libraries (specified in `stack.yaml`), and their dependencies. You can also specify which libraries to install, for example:
```bash
stack install ihaskell ihaskell-aeson ihaskell-diagrams
```

Mac OS X users using MacPorts may run into an [issue involving libiconv](http://blog.omega-prime.co.uk/?p=96). A solution is to add the following lines in the file stack.yaml:
```
extra-lib-dirs:
- /usr/lib
extra-include-dirs:
- /usr/include
```

#### Run IHaskell
Run IHaskell:
  - `ihaskell install` to install the IHaskell kernel into Jupyter.
  - `ipython notebook` for the browser-based interactive notebook.
  - `ipython console --kernel haskell` for a REPL.

If you've installed IHaskell in a sandbox, you will need to make sure that IPython can access the contents of the sandbox. You can do this via `cabal exec`:
```bash
cabal exec ipython -- notebook
```
Likewise, if you've installed IHaskell with `stack`:
```bash
stack exec ipython -- notebook
```

#### (Optional) Install Support Libraries

IHaskell comes with many support libraries, such as `ihaskell-diagrams`, `ihaskell-parsec`, and so on, which add rich and interactive displays for common libraries.
You can install these with `cabal install`. To install all of them, clone this repository and run `./build.sh all` to install IHaskell and all of its display support libraries.

You may run into some issues with installing the `cairo` dependency on Macs. To fix this, you can install `gcc` via `brew` and then use it to install `cairo`:
```bash
brew install gcc49
cabal install cairo --with-gcc=gcc-4.9
```

### Gotchas

These are simply some problems have had and solutions to them.

**Problem**: You have Anaconda or Enthought or some other python distribution, and for unknown reasons IHaskell just hangs after the first input.

**Solution**: Anaconda and Enthought cause problems. Get rid of them.

**Problem**: You get an error when `pyzmq` is compiling that looks somewhat like
```
cc1: error: -Werror=unused-command-line-argument-hard-error-in-future: No option -Wunused-command-line-argument-hard-error-in-future
```
**Solution:** Rerun the command after changing the `ARCHFLAGS` variable via
```bash
export ARCHFLAGS=-Wno-error=unused-command-line-argument-hard-error-in-future
```

**Problem**: You'd like to have IHaskell run some code every time it starts up, like `~/.ghci` or `~/.bashrc`.

**Solution**: IHaskell uses `~/.ihaskell/rc.hs` as its default configuration file; if you put code into that file (it may or may not exist), it will be loaded on startup. You can substitute a different file by passing the `--conf=myfile.hs` argument to `ihaskell install` to reconfigure the kernel.

**Note**: You may have some trouble due to browser caches with the notebook interface if you also use IPython's notebook interface or have used it in the past. If something doesn't work or IPython says it can't connect to the notebook server, make sure to clear the browser cache in whatever browser you're using, or try another browser.

# Contributing

IHaskell is a young project, and I'd love your help getting it to a stable and useful point. There's a lot to do, and if you'd like to contribute, feel free to get in touch with me via my email at andrew period gibiansky at gmail - although browsing the code should be enough to get you started, I'm more than happy to answer any questions myself.

**For package maintainers:** IHaskell has an ability to display data types it knows about with a rich format based on images or HTML. In order to do so, an external package `ihaskell-something` must be created and installed. Writing these packages is simply - they must just contain instance of the `IHaskellDisplay` typeclass, defined in `IHaskell.Display`, and for a package `ihaskell-something` should have a single module `IHaskell.Display.Something`. If you have a package with interesting data types that would benefit from a rich display format, please get in contact with me (andrew dot gibiansky at gmail) to write one of these packages! A sample package is available [here](https://github.com/gibiansky/IHaskell/tree/master/ihaskell-display/ihaskell-basic).

# Developer Notes

Before diving in, you should read the [brief description of IPython kernel architectures](http://andrew.gibiansky.com/blog/ipython/ipython-kernels/)
and read the [complete messaging protocol specification](http://ipython.org/ipython-doc/dev/development/messaging.html).


Please format your code with `hindent --style gibiansky` before submitting it; Travis CI automatically checks for code style before merging!

**Loading IHaskell into GHCi for testing:**

Use one of the methods below to access IHaskell files in GHCi. Once inside GHCi, you can load an IHaskell file; for example, `:load IHaskell/Config.hs`.

**Using cabal repl**

If you have the latest version of cabal (>v1.18.0), the simplest thing to do is 

```bash
cd <path-to-IHaskell>
cabal repl
```

This will hide all packages not listed in `IHaskell.cabal`

**Using GHCi directly**

If you don't want to use `cabal repl`, you can just call ghci which can read the `.ghci` file included in the repository for the options.

```bash
cd <path-to-IHaskell>
chmod 600 .ghci # trust the .ghci file
ghci
```
Then in the ghci session  you can type things like:

```
:set -package setenv
:load src/Hspec.hs
hspec parserTests
:browse IHaskell.Types
```
