IHaskell
===
IHaskell is an implementation of the [IPython](http://ipython.org) kernel protocol which allows you to use Haskell inside IPython frontends such as `qtconsole` and `notebook`.

The project works with the IPython shell:

![IPython Console](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-console.png)

As well as the IPython browser-based notebook interface:

![IPython Notebook](https://raw.github.com/gibiansky/IHaskell/master/images/ihaskell-notebook.png)

Installation
===

Download the package from the Github repository:
```bash
git clone https://github.com/gibiansky/IHaskell
```

Install ZeroMQ:
```bash
sudo apt-get install libzmq-dev # Ubuntu
brew install zeromq # Macs with Homebrew
```

Install the package:
```bash
cd IHaskell;
cabal install --only-dependencies;
cabal configure;
cabal build;
cabal install;
```

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
