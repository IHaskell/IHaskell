![IHaskell](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-logo.png)

IHaskell
===
IHaskell is an implementation of the [IPython](http://ipython.org) kernel protocol which allows you to use Haskell inside IPython frontends such as `qtconsole` and `notebook`.

The project works with the IPython shell:

![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)

As well as the IPython browser-based notebook interface:

![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)

Installation
===

IPython
---
Make sure you have [IPython](http://ipython.org/) version 1.0 or higher. IHaskell will not work with older versions of IPython.
```bash
ipython --version     # Should print 1.0.0 (or higher!)
```

Haskell and Cabal
---
You should also have GHC and modern Cabal:
```bash
ghc --numeric-version # Should be 7.6.3
cabal --version       # Should be 1.18.*
```
If you do not have GHC or Cabal, you should be able to install both via the 
[Haskell Platform](http://www.haskell.org/platform/). On Macs with Homebrew, you can do this via
```bash
# Macs with Homebrew only, if you don't have GHC or Cabal
brew install haskell-platform
cabal update && cabal install cabal-install
```
Use `cabal install cabal-install` to update Cabal if you still have version 1.16 instead of 1.18.

Also, in order to use executables which `cabal` installs, they must be in your path. Execute this in your shell or add it to your `~/.bashrc`:
```bash
export PATH=~/.cabal/bin:$PATH
```

ZeroMQ
---
Make sure that ZeroMQ 3 is installed. Eventually IHaskell will be ported to ZeroMQ 4, but for now it is on version 3.
Note that there are different instructions for different platforms:
```bash
# For Ubuntu (Saucy):
sudo apt-get install libzmq3-dev

# For Macs with Homebrew:
brew install zeromq
brew switch zeromq 3.2.4

# Compiling from source:
git clone git@github.com:zeromq/zeromq3-x.git libzmq
./autogen.sh && ./configure && make
sudo make install
sudo ldconfig
```

Compilation Tools
---
Install the `happy`
```bash
cabal install happy
cabal install cpphs
```

IHaskell Installation
---
Install the package from Hackage:
```bash
cabal install ihaskell
```

**Alternatively**, for the most recent version, you can install package from the Github repository and compile it from there:
```bash
git clone https://github.com/gibiansky/IHaskell
cd IHaskell
cabal install
```

Running IHaskell
---

Finally, run the notebook or console interface:
```bash
IHaskell notebook # Should open a browser window!
IHaskell console
```

There is a test notebook in the `IHaskell` directory.

**Important Note:** Using `IHaskell console` requires a proper patch to IPython. There is 
[a pull request](https://github.com/ipython/ipython/pull/4678) open on the IPython repository to fix this, but until it is merged, you need to install IPython from [this branch](https://github.com/ivanov/ipython/tree/console-display-text).

**Note**: You may have some trouble due to browser caches with the notebook interface if you also use IPython's notebook interface or have used it in the past. If something doesn't work or IPython says it can't connect to the notebook server, make sure to clear the browser cache in whatever browser you're using, or try another browser.

Contributing
===

IHaskell is a young project, and I'd love your help getting it to a stable and useful point. There's a lot to do, and if you'd like to contribute, feel free to get in touch with me via my email at andrew period gibiansky at gmail - although browsing the code should be enough to get you started, I'm more than happy to answer any questions myself.

**For package maintainers:** IHaskell has an ability to display data types it knows about with a rich format based on images or HTML. In order to do so, an external package `ihaskell-something` must be created and installed. Writing these packages is simply - they must just contain instance of the `IHaskellDisplay` typeclass, defined in `IHaskell.Display`, and for a package `ihaskell-something` should have a single module `IHaskell.Display.Something`. If you have a package with interesting data types that would benefit from a rich display format, please get in contact with me (andrew dot gibiansky at gmail) to write one of these packages! A sample package is available [here](https://github.com/gibiansky/IHaskell-Display).

Developer Notes
---

Before diving in, you should read the [brief description of IPython kernel architectures](http://andrew.gibiansky.com/blog/ipython/ipython-kernels/)
and read the [complete messaging protocol specification](http://ipython.org/ipython-doc/dev/development/messaging.html).

Skim the rather-lacking [Haddock documentation](http://gibiansky.github.io/IHaskell/IHaskell/).

Module Quickstart: 
- `Main`: Argument parsing and basic messaging loop, using Haskell Chans to communicate with the ZeroMQ sockets.
- `IHaskell.Types`: All message type definitions.
- `IHaskell.Eval.Evaluate`: Wrapper around GHC API, exposing a single `evaluate` interface that runs a statement, declaration, import, or directive.
- `IHaskell.IPython`: Shell scripting wrapper using `Shelly` for the `notebook`, `setup`, and `console` commands.
- `IHaskell.Message.Parser`: Parsing messages received from IPython.
- `IHaskell.Message.UUID`: UUID generator and data structure.
- `IHaskell.Message.Writer`: `ToJSON` for Messages.
- `IHaskell.ZeroMQ`: Low-level ZeroMQ communication wrapper. `serveProfile` starts listening on all necessary sockets, and returns a `ZeroMQInterface` record. This record exposes reading and writing `Chan Message` messages for all the necessary sockets, so then the rest of the application can simply use that interface.

First steps:

- Fork the repository on Github and clone your fork for editing. 
- Build IHaskell as follows:

```bash 
cd /path/to/IHaskell
cabal configure --enable-tests
cabal build
```

**Loading IHaskell into GHCi for testing:**

Use one of the methods below to access IHaskell files in GHCi. Once inside GHCi, you can load an IHaskell file; for example, `:load IHaskell/Config.hs`.

**Using cabal repl**

If you have the latest version of cabal (>v1.18.0), the simplest thing to do is 

```bash
cd <path-to-IHaskell>
cabal repl
```

The will hide all packages not listed in the
`IHaskell.cabal`

**Using GHCi directly**

If you don't want to use cabal repl, you can just call ghci with the appropriate options. You can find these in the IHaskell.cabal file. 

```bash
ghci -XDoAndIfThenElse -XNoImplicitPrelude -XOverloadedStrings -package ghc -optP-include -optPdist/build/autogen/cabal_macros.h
```

If you just call ghci, it will use the options present in the .ghci file that comes with the IHaskell repo. 
