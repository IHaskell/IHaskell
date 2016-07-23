IHaskell-Magic
================

Instances of IHaskellDisplay for `Text` and `ByteString`, where the actual
image or text format is determined using [libmagic](http://packages.debian.org/unstable/libdevel/libmagic-dev), which classifies files according to their contents. It is the same as the shell command `file(1)`. Depending on your OS, you will have to install the c-library first. On a debian-like OS:

```bash
apt-get install libmagic-dev
cd ihaskell-magic
cabal install
```

On OSX:

```bash
brew install libmagic
brew link libmagic
stack install ihaskell-magic --extra-lib-dirs=/usr/local/lib --extra-include-dirs=/usr/local/include
```

The instances provided allow displaying images and text with markup using just one line:
```haskell
import qualified Data.ByteString as B
import qualified Data.Text.IO as T
B.readFile "foo.png"
B.readFile "foo.svg"
B.readFile "foo.jpg" -- currently broken (Jan6,2014)
T.readFile "foo.tex" -- doesn't work that well for literal strings,
                     -- since you pretty much need a \documentclass[]{} to get
                     -- the file recognized, at which point I'm not sure it renders
T.readFile "foo.html"
```
While you can use `B.readFile "foo.tex"`, that involves more assumptions regarding encodings.
