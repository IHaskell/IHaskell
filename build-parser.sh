#!/bin/sh
cpphs --linepragma --text HaskellParser.y.pp -OParser.y
happy Parser.y
rm Parser.y
mv Parser.hs IHaskell/GHC/HaskellParser.hs
