
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
-- @ import IHaskell.Display.Graphviz @
-- @ dot "digraph { l -> o; o -> v; v -> e; h -> a ; a -> s; s -> k ; k -> e ; e -> l ; l -> l}" @
module IHaskell.Display.Graphviz (
    dot
  , neato
  , fdp
  , sfdp
  , circo
  , twopi
  , nop
  , nop2
  , osage
  , patchwork
  , Graphviz(..)
  ) where

import System.Process (readProcess)
import qualified Data.Text as T
import IHaskell.Display

-- | The body of a Graphviz program.
--
-- e.g. @ graph { a -- b } @
type GraphvizProgramBody = String

-- | Main Graphviz object.
data Graphviz
  = Dot !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Dot](https://graphviz.org/docs/layouts/dot/)
  | Neato !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Neato](https://graphviz.org/docs/layouts/neato/)
  | Fdp !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Fdp](https://graphviz.org/docs/layouts/fdp/)
  | Sfdp !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Sfdp](https://graphviz.org/docs/layouts/sfdp/)
  | Circo !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Circo](https://graphviz.org/docs/layouts/circo/)
  | Twopi !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Twopi](https://graphviz.org/docs/layouts/twopi/)
  | Nop !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Nop](https://graphviz.org/docs/layouts/nop/)
  | Nop2 !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Nop2](https://graphviz.org/docs/layouts/nop2/)
  | Osage !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Osage](https://graphviz.org/docs/layouts/osage/)
  | Patchwork !GraphvizProgramBody
  -- ^ A Graphviz plotted using [Patchwork](https://graphviz.org/docs/layouts/patchwork/)


-- | Create a 'Graphviz' using 'dot'.
dot :: GraphvizProgramBody -> Graphviz
dot = Dot

-- | Create a 'Graphviz' using 'neato'.
neato :: GraphvizProgramBody -> Graphviz
neato = Neato

-- | Create a 'Graphviz' using 'fdp'.
fdp :: GraphvizProgramBody -> Graphviz
fdp = Fdp

-- | Create a 'Graphviz' using 'sfdp'.
sfdp :: GraphvizProgramBody -> Graphviz
sfdp = Sfdp

-- | Create a 'Graphviz' using 'circo'.
circo :: GraphvizProgramBody -> Graphviz
circo = Circo

-- | Create a 'Graphviz' using 'twopi'.
twopi :: GraphvizProgramBody -> Graphviz
twopi = Twopi

-- | Create a 'Graphviz' using 'nop'.
nop :: GraphvizProgramBody -> Graphviz
nop = Nop

-- | Create a 'Graphviz' using 'nop2'.
nop2 :: GraphvizProgramBody -> Graphviz
nop2 = Nop2

-- | Create a 'Graphviz' using 'osage'.
osage :: GraphvizProgramBody -> Graphviz
osage = Osage

-- | Create a 'Graphviz' using 'patchwork'.
patchwork :: GraphvizProgramBody -> Graphviz
patchwork = Patchwork

instance IHaskellDisplay Graphviz where
  display fig = do
    svgDisp <- graphDataSVG fig
    return $ Display [svgDisp]

graphDataSVG :: Graphviz -> IO DisplayData
graphDataSVG prog = svg . T.pack <$> case prog of
  Dot dotBody -> readProcess "dot" ["-Tsvg"] dotBody
  Neato dotBody -> readProcess "neato" ["-Tsvg"] dotBody
  Fdp dotBody -> readProcess "fdp" ["-Tsvg"] dotBody
  Sfdp dotBody -> readProcess "sfdp" ["-Tsvg"] dotBody
  Circo dotBody -> readProcess "circo" ["-Tsvg"] dotBody
  Twopi dotBody -> readProcess "twopi" ["-Tsvg"] dotBody
  Nop dotBody -> readProcess "nop" ["-Tsvg"] dotBody
  Nop2 dotBody -> readProcess "nop2" ["-Tsvg"] dotBody
  Osage dotBody -> readProcess "osage" ["-Tsvg"] dotBody
  Patchwork dotBody -> readProcess "patchwork" ["-Tsvg"] dotBody
