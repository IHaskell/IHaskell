![IHaskell](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-logo.png)

IHaskell
===
IHaskell is an implementation of the [IPython](http://ipython.org) kernel protocol which allows you to use Haskell inside IPython frontends such as `qtconsole` and `notebook`.

[Demo Notebook](http://gibiansky.github.io/IHaskell/demo.html)

**_Please_ run `cabal update` before installing - IHaskell updates _very_ regularly!**

---

The project works with the IPython shell:

![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)

As well as the IPython browser-based notebook interface:

![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)

# More usage information on the [wiki](https://github.com/gibiansky/IHaskell/wiki).

Kronos Haskell
===
For Mac OS X 10.9 and higher, you can install IHaskell via [Kronos Haskell](http://www.kronosnotebook.com/static/haskell.html).
Kronos Haskell is a packaged interface for IHaskell, downloadable as a Mac app
– download it, put it in /Applications, and run it as any other Mac app.

Docker Installation (Linux, Mac and Windows)
===

There is a Docker container for IHaskell.  It can be run in Linux, Mac OS as well as Windows, although Mac OS and Windows users will need to install [boot2docker](https://github.com/boot2docker/boot2docker). Instructions for installing on your platform can be found on [docker.com](https://docs.docker.com/installation/).

After installing docker, type the following to run IHaskell in your browser.

```bash
docker run -p 8778:8778 -v $(pwd):/home/haskell/.ihaskell/notebooks gregweber/ihaskell
```

This will share the current working directory with the Docker container.

Visit [http://0.0.0.0:8778/](http://0.0.0.0:8778/) to start using IHaskell.

If you are using boot2docker you may need to visit [http://192.168.59.103:8778/](http://192.168.59.103:8778/) instead.

You can also run the IHaskell in the console with

```bash
docker run --rm -i -t gregweber/ihaskell console
```

*Note*: Linux users might need to type `sudo` before this command, unless they have added themselves to the `docker` group.  Please see this [stackexchange](http://askubuntu.com/a/477554) answer for more information.

Windows Installation
===

Install Virtualbox. Load a Linux distro and install Docker, then run the Docker package.
This also works on Mac, but Mac users may prefer installing from source.

Source Installation (Mac and Linux)
===

If you have any trouble with installation or have other questions about IHaskell, feel free to open an issue [on Github](https://github.com/gibiansky/IHaskell/issues?direction=desc&sort=updated&state=open) or join our IRC at #ihaskell on chat.freenode.net.

IHaskell is built on top of IPython, which provides the frontends and the entire infrastructure. By default, IHaskell will attempt to use the system IPython installation. If it fails to find one, it will create its own virtualenv and install all its own Python dependencies (this happens on the first run). However, if you want to provide your own IPython to override the automatic behaviour, you can instead pass IHaskell the `--ipython /path/to/exec/ipython` flag, in which case the installation will not happen and it'll just use the IPython you passed it. You *must* have IPython 2.0, though; older versions will *not* work properly!  Installing IPython 2.0 from PyPI is recommended (as this is how the automatic installer does it).

*Note that since the default install  uses virtualenv, your system must be virtualenv compatibile. Mac users have run into [this issue](http://stackoverflow.com/questions/5904319/problem-with-virtualenv-in-mac-os-x) before, for example.*

ZeroMQ
---
IHaskell uses a library known as ZeroMQ for asynchronous communication. Make sure that ZeroMQ 4 is installed - it is a relatively recent library and thus you may have older versions installed.

For Macs, you can easily install it with [Homebrew](http://brew.sh/):
```bash
# For Macs with Homebrew:
brew update
brew install zeromq
```
(If using 32-bit Haskell Platform, you *may* need to use `brew install zeromq --universal`. YMMV.)

On other platforms, you can easily install from source:
```bash
# Compiling from source:
git clone git@github.com:zeromq/zeromq4-x.git libzmq
cd libzmq
./autogen.sh && ./configure && make
sudo make install
sudo ldconfig
```

If your own platform has a package and I haven't included instructions for it, feel free to send me an email or a PR on this README.

Haskell and Cabal
---
You should also have GHC and modern Cabal:
```bash
ghc --numeric-version # Should be 7.6.3, 7.8.2, or 7.8.3
cabal --version       # Should be 1.18.* or newer
```
Since IHaskell uses the GHC API for evaluation and parsing, other versions of GHC may not work.

If you do not have GHC or Cabal, you should be able to install both via the 
[Haskell Platform](http://www.haskell.org/platform/). On Macs with Homebrew, you can do this via
```bash
# Macs with Homebrew only, if you don't have GHC or Cabal
brew install ghc cabal-install
cabal update && cabal install cabal-install
```
Use `cabal install cabal-install` to update Cabal if you still have version 1.16 instead of 1.18.

Also, in order to use executables which `cabal` installs, they must be in your path. Execute this in your shell or add it to your `~/.bashrc`:
```bash
# If you have a ~/.cabal/bin folder:
export PATH=~/.cabal/bin:$PATH

# If you have a ~/Library/Haskell/bin folder on OS X:
export PATH=~/Library/Haskell/bin:$PATH
```

Some of the IHaskell packages depend on `cairo`, such as `ihaskell-diagrams`, where `cairo` is used for drawing the displays. If you are using Mac OS X 10.9 (Mavericks) you will need to compile
cairo with gcc, not clang. This can be done as follows:

```bash
brew install gcc48
cabal install cairo --with-gcc=gcc-4.8
```
Note that this is only necessary if you want to use the `ihaskell-display` packages provided.

Compilation Tools
---
Install the `happy` parser generator tool and `cpphs` preprocessor:
```bash
cabal install happy cpphs
```

Python Installation
---

IHaskell will automatically install python tools itself.
If you already have ipython installed you can use the --ipython flag.

On Linux make sure you also have `python-dev` (or equivalent) installed, which is needed to install `pyzmq`

IHaskell Installation
---

Install the IHaskell package from Hackage:
```bash
cabal update # Please do this! IHaskell updates *very* frequently.
cabal install ihaskell --reorder-goals
```
The `--reorder-goals` flag is necessary due to a bug in `cabal-install`. The `--solver=topdown` also seems to do the trick.

If you have trouble with this installation, please make sure to try the Github version first. IHaskell is in rapid development, so Hackage may not always be the most recent. In order to install from Github, pull from the repository, and then install with
```bash
./build.sh all
```
Note that you should *not* use `cabal install`, as you want to also install dependencies from the Github source, and not Hackage. `build.sh` is a simple included script which manages this for you.

Running IHaskell
---

Finally, run the notebook or console interface:
```bash
IHaskell notebook # Should open a browser window!
IHaskell console
```

#### Gotchas

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

If you want to pass specific options to the underlying IPython, you can do it like this:
```bash
# Expose the notebook on all ports!
IHASKELL_IPYTHON_ARGS="--ip=*" ihaskell notebook
```
Anything in `IHASKELL_IPYTHON_ARGS` will be passed to the underlying `ipython` instance as an argument when you run IHaskell.

There is a test notebook in the `IHaskell` directory. To try it, run IHaskell with `IHaskell notebook --serve=IHaskell`.

If you get a pip error while IHaskell is installing Python
dependencies on the first run (it will look like
`EOFError: EOF when reading a line`), then you can do the following
after ensuring that pip >= 1.5 is installed:
```bash
env PIP_EXISTS_ACTION=w IHaskell notebook
```

If it *still* doesn't work and you are using Mac OS X then
`brew upgrade python`. Some older versions of Homebrew's Python have a
broken version of distutils. Homebrew's version of Python 2.7.6 *does* work.

**Problem**: You'd like to have IHaskell run some code every time it starts up, like `~/.ghci` or `~/.bashrc`.

**Solution**: IHaskell uses `~/.ihaskell/rc.hs` as its default configuration file; if you put code into that file (it may or may not exist), it will be loaded on startup. You can substitute a different file by passing the `--conf=myfile.hs` argument to IHaskell,
as in `IHaskell notebook --conf=/home/user/.ihaskellrc.hs`.

**Note**: You may have some trouble due to browser caches with the notebook interface if you also use IPython's notebook interface or have used it in the past. If something doesn't work or IPython says it can't connect to the notebook server, make sure to clear the browser cache in whatever browser you're using, or try another browser.

Contributing
===

IHaskell is a young project, and I'd love your help getting it to a stable and useful point. There's a lot to do, and if you'd like to contribute, feel free to get in touch with me via my email at andrew period gibiansky at gmail - although browsing the code should be enough to get you started, I'm more than happy to answer any questions myself.

**For package maintainers:** IHaskell has an ability to display data types it knows about with a rich format based on images or HTML. In order to do so, an external package `ihaskell-something` must be created and installed. Writing these packages is simply - they must just contain instance of the `IHaskellDisplay` typeclass, defined in `IHaskell.Display`, and for a package `ihaskell-something` should have a single module `IHaskell.Display.Something`. If you have a package with interesting data types that would benefit from a rich display format, please get in contact with me (andrew dot gibiansky at gmail) to write one of these packages! A sample package is available [here](https://github.com/gibiansky/IHaskell/tree/master/ihaskell-display/ihaskell-basic).

Developer Notes
---

Before diving in, you should read the [brief description of IPython kernel architectures](http://andrew.gibiansky.com/blog/ipython/ipython-kernels/)
and read the [complete messaging protocol specification](http://ipython.org/ipython-doc/dev/development/messaging.html).

Skim the rather-lacking [Haddock documentation](http://gibiansky.github.io/IHaskell/IHaskell/).

First steps:

- Fork the repository on Github and clone your fork for editing. 

**option 1** 
```bash 
cd /path/to/IHaskell
./build.sh
```
**option 2**
```bash
cd /path/to/IHaskell
cabal sandbox init
cabal sandbox add-source ihaskell-display/* ghc-parser
cabal install IHaskell $(basename ihaskell-display/*)
```

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
