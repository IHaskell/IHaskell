#!/bin/sh
cpphs --linepragma --text HaskellParser.y.pp -OParser.y
happy Parser.y
rm Parser.y
mkdir -p IHaskell/GHC
mv Parser.hs IHaskell/GHC/HaskellParser.hs
