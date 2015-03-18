#!/usr/bin/env python3

from __future__ import print_function

import sys
import os
import subprocess


def hindent(contents):
    return subprocess.check_output(["hindent", "--style", "gibiansky"],
                                   input=bytes(contents, 'utf-8'))


def diff(src1, src2):

    with open(".tmp1", "w") as f1:
        f1.write(src1)

    with open(".tmp2", "w") as f2:
        f2.write(src2)

    return subprocess.check_output(["diff", ".tmp1", ".tmp2"])

# Verify that we're in the right directory
try:
    open("ihaskell.cabal", "r").close()
except:
    print(sys.argv[0], "must be run from the ihaskell directory",
          file=sys.stderr)

# Find all the source files
sources = []
for root, dirnames, filenames in os.walk("src"):
    for filename in filenames:
        if filename.endswith(".hs"):
            sources.append(os.path.join(root, filename))


hindent_outputs = {}
for source_file in sources:
    print("Formatting file", source_file)
    with open(source_file, "r") as f:
        original_source = f.read()
    formatted_source = hindent(original_source)

    hindent_outputs[source_file] = (original_source, formatted_source)

diffs = {filename: diff(original, formatted)
         for (filename, (original, formatted)) in hindent_outputs.values()}

for filename, diff in diffs.items():
    print(filename)
    print('=' * 10)
    print(diff)
