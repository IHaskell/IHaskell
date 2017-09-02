#!/bin/bash -e
# Called from Setup.hs.

function make_parser {
    SRCDIR=$1
    SRCNAME=$2

    # Preprocess the GHC parser we're using to CPP subs.
    cpphs --linepragma --text ${SRCNAME}.y.pp -OParser.y

    # Compile the parser and remove intermediate file.
    happy Parser.y
    rm Parser.y

    # Move output Haskell to source directory.
    mkdir -p $SRCDIR/Language/Haskell/GHC
    mv Parser.hs $SRCDIR/Language/Haskell/GHC/HappyParser.hs
}
