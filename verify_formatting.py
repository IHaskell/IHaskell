#!/usr/bin/env python3

from __future__ import print_function

import sys
import os
import subprocess


def hindent(contents):
    with open(".tmp3", "w") as f:
        f.write(contents)
    with open(".tmp3", "r") as f:
        output = subprocess.check_output(["hindent", "--style", "gibiansky"],
                                         stdin=f)
    return output.decode('utf-8')


def diff(src1, src2):
    # Ignore trailing newlines
    if src1[-1] == "\n":
        src1 = src1[:-1]
    if src2[-1] == "\n":
        src2 = src2[:-1]

    with open(".tmp1", "w") as f1:
        f1.write(src1)

    with open(".tmp2", "w") as f2:
        f2.write(src2)

    try:
        output = subprocess.check_output(["diff", ".tmp1", ".tmp2"])
        return output.decode('utf-8')
    except subprocess.CalledProcessError as e:
        return e.output.decode('utf-8')

# Verify that we're in the right directory
try:
    open("ihaskell.cabal", "r").close()
except:
    print(sys.argv[0], "must be run from the ihaskell directory",
          file=sys.stderr)

# Find all the source files
sources = []
widget_dir = "ihaskell-display/ihaskell-widgets/src/IHaskell/Display/Widgets"
for source_dir in ["src", "ipython-kernel", "ihaskell-display"]:
    for root, dirnames, filenames in os.walk(source_dir):
        # Skip cabal dist directories
        if "dist" in root:
            continue

        # Ignore IHaskellPrelude.hs, it uses CPP in weird places
        if widget_dir in root:
            ignored_files = ["Types.hs"]
        else:
            ignored_files = ["IHaskellPrelude.hs"]
        for filename in filenames:
            if filename.endswith(".hs") and filename not in ignored_files:
                sources.append(os.path.join(root, filename))


hindent_outputs = {}
for source_file in sources:
    print("Formatting file", source_file)
    with open(source_file, "r") as f:
        original_source = f.read()
    formatted_source = hindent(original_source)

    hindent_outputs[source_file] = (original_source, formatted_source)

diffs = {filename: diff(original, formatted)
         for (filename, (original, formatted)) in hindent_outputs.items()}

incorrect_formatting = False
for filename, diff in diffs.items():
    if diff:
        incorrect_formatting = True
        print('Incorrect formatting in', filename)
        print('=' * 10)
        print(diff)

if incorrect_formatting:
    sys.exit(1)
