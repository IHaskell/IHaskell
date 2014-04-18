{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  IHaskellWidget(..),
  plain, html, png, jpg, svg, latex,
  serializeDisplay,
  Width, Height, Base64(..),
  encode64, base64,
  Display(..),
  DisplayData(..),
  printDisplay,

  -- Internal only use
  displayFromChan,
  Widget(..),
  ) where

import ClassyPrelude hiding (atomically) -- a lifted version
import Data.Serialize as Serialize
import Data.ByteString hiding (map, pack)
import Data.String.Utils (rstrip) 
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char
import Data.Bits.Utils (c2w8)
import Data.Aeson (Value)

import Control.Concurrent.STM.TChan
import Control.Monad.STM
import System.IO.Unsafe (unsafePerformIO)

import IHaskell.Types

type Base64 = Text

-- | these instances cause the image, html etc. which look like:
--
-- > Display
-- > [Display]
-- > IO [Display]
-- > IO (IO Display)
--
-- be run the IO and get rendered (if the frontend allows it) in the pretty
-- form.
instance IHaskellDisplay a => IHaskellDisplay (IO a) where
  display = (display =<<)

instance IHaskellDisplay Display where
  display = return

instance IHaskellDisplay DisplayData where
  display disp = return $ Display [disp]

instance IHaskellDisplay a => IHaskellDisplay [a] where
  display disps = do
    displays <- mapM display disps
    return $ ManyDisplay displays

-- | Encode many displays into a single one. All will be output.
many :: [Display] -> Display
many = ManyDisplay

-- | Generate a plain text display.
plain :: String -> DisplayData
plain = DisplayData PlainText . Char.pack . rstrip

-- | Generate an HTML display.
html :: String -> DisplayData
html = DisplayData MimeHtml . Char.pack

-- | Genreate an SVG display.
svg :: String -> DisplayData
svg = DisplayData MimeSvg . Char.pack

-- | Genreate a LaTeX display.
latex :: String -> DisplayData
latex = DisplayData MimeLatex . Char.pack

-- | Generate a PNG display of the given width and height. Data must be
-- provided in a Base64 encoded manner, suitable for embedding into HTML.
-- The @base64@ function may be used to encode data into this format.
png :: Width -> Height -> Base64 -> DisplayData
png width height = DisplayData (MimePng width height) . encodeUtf8

-- | Generate a JPG display of the given width and height. Data must be
-- provided in a Base64 encoded manner, suitable for embedding into HTML.
-- The @base64@ function may be used to encode data into this format.
jpg :: Width -> Height -> Base64 -> DisplayData
jpg width height = DisplayData (MimeJpg width height) . encodeUtf8

-- | Convert from a string into base 64 encoded data.
encode64 :: String -> Base64
encode64 str = base64 $ Char.pack str

-- | Convert from a ByteString into base 64 encoded data.
base64 :: ByteString -> Base64
base64 = decodeUtf8 . Base64.encode

-- | For internal use within IHaskell.
-- Serialize displays to a ByteString.
serializeDisplay :: Display -> ByteString
serializeDisplay = Serialize.encode

-- | Items written to this chan will be included in the output sent
-- to the frontend (ultimately the browser), the next time IHaskell
-- has an item to display.
{-# NOINLINE displayChan #-}
displayChan :: TChan Display
displayChan = unsafePerformIO newTChanIO

-- | Take everything that was put into the 'displayChan' at that point
-- out, and make a 'Display' out of it.
displayFromChan :: IO (Maybe Display)
displayFromChan =
  Just . many <$> unfoldM (atomically $ tryReadTChan displayChan)

-- | This is unfoldM from monad-loops. It repeatedly runs an IO action
-- until it return Nothing, and puts all the Justs in a list.
-- If you find yourself using more functionality from monad-loops, just add
-- the package dependency instead of copying more code from it.
unfoldM :: IO (Maybe a) -> IO [a]
unfoldM f = maybe (return []) (\r -> (r:) <$> unfoldM f) =<< f

-- | Write to the display channel. The contents will be displayed in the
-- notebook once the current execution call ends.
printDisplay :: IHaskellDisplay a => a -> IO ()
printDisplay disp = display disp >>= atomically . writeTChan displayChan
