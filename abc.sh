#/bin/bash sh

if hash ihaskell 2>/dev/null; then
  ihaskell install 2>/dev/null || echo "\"ihaskell install\" - failed, required ipython --version == 3.0.0 is $(ipython --version)"
else
  echo "tip, don't forget run: ihaskell install"
fi
