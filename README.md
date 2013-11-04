IHaskell
===
IHaskell is an implementation of the [IPython](http://ipython.org) kernel protocol which allows you to use Haskell inside IPython frontends such as `qtconsole` and `notebook`.

The project works with the IPython shell:

![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)

As well as the IPython browser-based notebook interface:

![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)

Installation
===

Make sure you have IPython version 1.0 or higher. IHaskell will not work with older versions of IPython.
```bash
ipython --version # Should print 1.0.0 (or higher!)
```

Download the package from the Github repository:
```bash
git clone https://github.com/gibiansky/IHaskell
```

Install ZeroMQ:
```bash
sudo apt-get install libzmq3-dev # Ubuntu (Saucy only)
brew install zeromq # Macs with Homebrew
```
(For older versions of Ubuntu, you should be able to download the ZeroMQ3 source and install without much difficulty.)

Install Happy:
```bash
sudo apt-get install happy # Ubuntu
```

Install the package:
```bash
cd IHaskell;
cabal install;
```
If you do not have GHC or Cabal, you should be able to install both via the [Haskell Platform](http://www.haskell.org/platform/).


Create the IPython profile:
```bash
IHaskell setup
```

Run the notebook or console interface:
```bash
IHaskell notebook # Should open a browser window!
IHaskell console
```

There is a test notebook in the `IHaskell` directory.

Contributing
===

IHaskell is an extremely young project, and I'd love your help getting it to a stable and useful point. There's a lot to do, and if you'd like to contribute, feel free to get in touch with me via my email at andrew period gibiansky at gmail - although browsing the code should be enough to get you started, I'm more than happy to answer any questions myself.

Some ideas for improvements:
- Type annotations. When a statement is evaluated, the GHC API returns the names of all bound variables. It should be possible to take those names and find the types of the variables, and display them in a table via the `display_data` message. 
- Implementing useful directives. Currently, support for GHCi-style ":"-initiated directives exist, but they do not do anything (and are instead just printed in green). Useful directives such as ":t" and ":i" and ":m [+-]" have yet to be implemented, and adding them would be a good way to get started with the codebase.
- Parsing and viewing of formats via `display_data` and HTML:
    - `aeson` compatibility which displays JSON as syntax highlighted JSON code via HTML.
    - Support for `repa` or `hmatrix` vectors and matrices being displayed.
    - `A custom typeclass for displaying data types as HTML, similar to Show.

Take a look at the [developer notes](https://github.com/gibiansky/IHaskell/blob/master/README.md#developer-notes) as well - they are sparse but may be helpful.

Developer Notes
===

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
cabal configure
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
