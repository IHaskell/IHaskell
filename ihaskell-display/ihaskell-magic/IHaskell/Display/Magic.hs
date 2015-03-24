{-# LANGUAGE ViewPatterns, TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Magic () where

import           IHaskell.Display
import           Magic
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.UTF8 as B

import           Text.Read
import           Data.Char

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           IHaskell.IPython.Types (MimeType(MimeSvg))
import           Data.ByteString.UTF8

instance IHaskellDisplay T.Text where
  display = display . T.encodeUtf8

instance IHaskellDisplay B.ByteString where
  display x = do
    m <- magicOpen []
    magicLoadDefault m
    f <- B.unsafeUseAsCStringLen x (magicCString m)
    return $ Display [withClass (parseMagic f) x]

b64 :: B.ByteString -> String
b64 = Char.unpack . Base64.encode

withClass :: MagicClass -> B.ByteString -> DisplayData
withClass SVG = DisplayData MimeSvg . T.decodeUtf8
withClass (PNG w h) = png w h . T.decodeUtf8 . Base64.encode
withClass JPG = jpg 400 300 . T.decodeUtf8 . Base64.encode
withClass HTML = html . B.toString
withClass LaTeX = latex . B.toString
withClass _ = plain . B.toString

{- | parse the string produced by magic.

>>> parseMagic "LaTeX 2e document, ASCII text, with very long lines"
LaTeX

>>> parseMagic "PNG image data, 480 x 480, 8-bit/color RGB, non-interlaced"
PNG 480 480

>>> parseMagic "HTML document, ASCII text, with very long lines"
HTML

>>> parseMagic "JPEG image data, JFIF standard 1.01"
JPG

-}
parseMagic :: String -> MagicClass
parseMagic f =
  case words f of
    "SVG":_                                                                                 -> SVG
    "PNG":_image:_data:(readMaybe -> Just w):_x:(readMaybe . takeWhile isDigit -> Just h):_ -> PNG w
                                                                                                 h
    "LaTeX":_                                                                               -> LaTeX
    "HTML":_                                                                                -> HTML
    "JPEG":_                                                                                -> JPG
    _                                                                                       -> Unknown

data MagicClass = SVG
                | PNG Int Int
                | JPG
                | HTML
                | LaTeX
                | Unknown
  deriving Show
