
-- | A module to help displaying information using the amazing Graphviz
-- (https://www.graphviz.org/) graph layouts.
--
-- You need to install and have graphviz tools available in your environment.
-- 
-- Currently only 'dot' is provided as a proof-of-concept.  This module may be
-- split in two (a package as an helper to Graphviz command line and this
-- package to provide 'IHaskellDisplay' instances.
--
-- Minimal notebook example:
--
-- @ :extension OverloadedStrings @
-- @ import IHaskell.Display.Graphviz @
-- @ dot LeftToRight "digraph { l -> o; o -> v; v -> e; h -> a ; a -> s; s -> k ; k -> e ; e -> l ; l -> l}" @
module IHaskell.Display.Graphviz (
    dot
  , RankDir(..)
  , Graphviz
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char
import           System.Process (readProcess)
import           IHaskell.Display

-- | The body of a Graphviz program.
--
-- e.g. @ graph { a -- b } @
type GraphvizProgramBody = ByteString

-- | Plot direction when laying out with Dot.
data RankDir = LeftToRight | TopToBottom
  deriving (Show, Eq, Ord)

-- | Main Graphviz object.
data Graphviz =
  Dot !RankDir !ByteString
  -- ^ A Graphviz plotted using Dot (only available currently).

-- | Create a 'Graphviz' using 'dot'.
dot :: RankDir -> GraphvizProgramBody -> Graphviz
dot = Dot

instance IHaskellDisplay Graphviz where
  display fig = do
    pngDisp <- graphDataPNG fig
    return $ Display [pngDisp]

rankdirOpt :: RankDir -> String
rankdirOpt LeftToRight = "-Grankdir=LR"
rankdirOpt TopToBottom = "-Grankdir=TB"

name = "ihaskell-graphviz."

-- Width and height
w = 300
h = 300

graphDataPNG :: Graphviz -> IO DisplayData
graphDataPNG (Dot rankdir dotBody) = do
  switchToTmpDir

  let fname = name ++ "png"
  -- Write the image.
  ret <- readProcess "dot" ["-Tpng", "-o", fname, rankdirOpt rankdir] (Char.unpack dotBody)

  -- Force strictness on readProcess, read file, and convert to base64.
  imgData <- seq (length ret) $ Char.readFile fname
  return $ png w h $ base64 imgData
