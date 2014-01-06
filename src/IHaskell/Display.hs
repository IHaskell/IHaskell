{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  plain, html, png, jpg, svg, latex,
  serializeDisplay,
  Width, Height, Base64,
  encode64, base64,
  DisplayData
  ) where

import ClassyPrelude
import Data.Serialize as Serialize
import Data.ByteString
import Data.String.Utils (rstrip) 
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char

import IHaskell.Types

type Base64 = ByteString

-- | A class for displayable Haskell types.
--
-- IHaskell's displaying of results behaves as if these two
-- overlapping/undecidable instances also existed:
-- 
-- > instance (Show a) => IHaskellDisplay a
-- > instance Show a where shows _ = id
class IHaskellDisplay a where
  display :: a -> IO [DisplayData]

-- | Generate a plain text display.
plain :: String -> DisplayData
plain = Display PlainText . Char.pack . rstrip

-- | Generate an HTML display.
html :: String -> DisplayData
html = Display MimeHtml . Char.pack

-- | Genreate an SVG display.
svg :: String -> DisplayData
svg = Display MimeSvg . Char.pack

-- | Genreate a LaTeX display.
latex :: String -> DisplayData
latex = Display MimeLatex . Char.pack

-- | Generate a PNG display of the given width and height. Data must be
-- provided in a Base64 encoded manner, suitable for embedding into HTML.
-- The @base64@ function may be used to encode data into this format.
png :: Width -> Height -> Base64 -> DisplayData
png width height = Display (MimePng width height)

-- | Generate a JPG display of the given width and height. Data must be
-- provided in a Base64 encoded manner, suitable for embedding into HTML.
-- The @base64@ function may be used to encode data into this format.
jpg :: Width -> Height -> Base64 -> DisplayData
jpg width height = Display (MimeJpg width height)

-- | Convert from a string into base 64 encoded data.
encode64 :: String -> Base64
encode64 str = base64 $ Char.pack str

-- | Convert from a ByteString into base 64 encoded data.
base64 :: ByteString -> Base64
base64 = Base64.encode

-- | For internal use within IHaskell.
-- Serialize displays to a ByteString.
serializeDisplay :: [DisplayData] -> ByteString
serializeDisplay = Serialize.encode
