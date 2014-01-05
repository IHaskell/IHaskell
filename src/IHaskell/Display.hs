{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  plain, html, png, jpg, svg, latex,
  serializeDisplay,
  Width, Height, Base64Data
  ) where

import ClassyPrelude
import Data.Serialize as Serialize
import Data.ByteString
import Data.String.Utils (rstrip) 

import IHaskell.Types

type Base64Data = String

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
plain = Display PlainText . rstrip

-- | Generate an HTML display.
html :: String -> DisplayData
html = Display MimeHtml

png :: Width -> Height -> Base64Data -> DisplayData
png width height = Display (MimePng width height)

jpg :: Width -> Height -> Base64Data -> DisplayData
jpg width height = Display (MimeJpg width height)

svg :: String -> DisplayData
svg = Display MimeSvg

latex :: String -> DisplayData
latex = Display MimeLatex

serializeDisplay :: [DisplayData] -> ByteString
serializeDisplay = Serialize.encode
