{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  plain, html, png, jpg, svg, latex,
  serializeDisplay,
  Width, Height, Base64,
  encode64, base64,
  Display(..),
  DisplayData(..),
  displayDyn,
  displayFromDyn,
  displayChan,
  displayFromChan,
  ) where

import ClassyPrelude
import Data.Serialize as Serialize
import Data.ByteString hiding (map)
import Data.String.Utils (rstrip) 
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as Char

import Data.Dynamic
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import IHaskell.Types

import Control.Concurrent.STM.TChan
import Control.Monad.STM

type Base64 = ByteString

-- | A class for displayable Haskell types.
--
-- IHaskell's displaying of results behaves as if these two
-- overlapping/undecidable instances also existed:
-- 
-- > instance (Show a) => IHaskellDisplay a
-- > instance Show a where shows _ = id
class IHaskellDisplay a where
  display :: a -> IO Display

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

-- to be tried first: if it gives Nothing (or doesn't typecheck due to no
-- Typeable instance), try IHaskellDisplay
displayFromDyn x = do
     fs <- readMVar displayDyn 
     let go [] = return Nothing
         go (f:fs) = do
            fx <- f (toDyn x)
            case fx of
                First Nothing -> go fs
                First (Just y) -> return (Just y)
     Just x <- go fs
     display x

{-# NOINLINE displayDyn #-}
displayDyn :: MVar [Dynamic -> IO (First Display)]
displayDyn = unsafePerformIO (newMVar [])

{-# NOINLINE displayChan #-}
displayChan :: TChan Display
displayChan = unsafePerformIO newTChanIO


displayFromChan :: IO Display
displayFromChan = do
    many <$> unfoldM (atomically (tryReadTChan displayChan))

-- | unfoldM in monad-loops
unfoldM :: IO (Maybe a) -> IO [a]
unfoldM f = maybe (return []) (\r -> (r:) <$> unfoldM f) =<< f

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
png width height = DisplayData (MimePng width height)

-- | Generate a JPG display of the given width and height. Data must be
-- provided in a Base64 encoded manner, suitable for embedding into HTML.
-- The @base64@ function may be used to encode data into this format.
jpg :: Width -> Height -> Base64 -> DisplayData
jpg width height = DisplayData (MimeJpg width height)

-- | Convert from a string into base 64 encoded data.
encode64 :: String -> Base64
encode64 str = base64 $ Char.pack str

-- | Convert from a ByteString into base 64 encoded data.
base64 :: ByteString -> Base64
base64 = Base64.encode

-- | For internal use within IHaskell.
-- Serialize displays to a ByteString.
serializeDisplay :: Display -> ByteString
serializeDisplay = Serialize.encode
