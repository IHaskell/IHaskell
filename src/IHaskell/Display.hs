{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}

-- | If you are interested in the IHaskell library for the purpose of augmenting the IHaskell
-- notebook or writing your own display mechanisms and widgets, this module contains all functions
-- you need.
--
-- In order to create a display mechanism for a particular data type, write a module named (for
-- example) @IHaskell.Display.YourThing@ in a package named @ihaskell-yourThing@. (Note the
-- capitalization - it's important!) Then, in that module, add an instance of @IHaskellDisplay@ for
-- your data type. Similarly, to create a widget, add an instance of @IHaskellWidget@.
--
-- An example of creating a display is provided in the
-- <http://gibiansky.github.io/IHaskell/demo.html demo notebook>.
--
module IHaskell.Display (
    -- * Rich display and interactive display typeclasses and types
    IHaskellDisplay(..),
    Display(..),
    DisplayData(..),
    IHaskellWidget(..),

    -- ** Interactive use functions
    printDisplay,

    -- * Constructors for displays
    plain,
    html,
    png,
    jpg,
    svg,
    latex,
    javascript,
    many,

    -- ** Image and data encoding functions
    Width,
    Height,
    Base64(..),
    encode64,
    base64,

    -- ** Utilities
    switchToTmpDir,

    -- * Internal only use
    displayFromChanEncoded,
    serializeDisplay,
    Widget(..),
    ) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Data.Serialize as Serialize
import qualified Data.ByteString.Base64 as Base64
import           Data.Aeson (Value)
import           System.Directory (getTemporaryDirectory, setCurrentDirectory)

import           Control.Concurrent.STM (atomically)
import           Control.Exception (try)
import           Control.Concurrent.STM.TChan
import           System.IO.Unsafe (unsafePerformIO)

import qualified Data.Text.Encoding as E

import           IHaskell.Types
import           IHaskell.Eval.Util (unfoldM)
import           StringUtils (rstrip)

type Base64 = Text

-- | these instances cause the image, html etc. which look like:
--
-- > Display
-- > [Display]
-- > IO [Display]
-- > IO (IO Display)
--
-- be run the IO and get rendered (if the frontend allows it) in the pretty form.
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
plain = DisplayData PlainText . T.pack . rstrip

-- | Generate an HTML display.
html :: String -> DisplayData
html = DisplayData MimeHtml . T.pack

-- | Generate an SVG display.
svg :: String -> DisplayData
svg = DisplayData MimeSvg . T.pack

-- | Generate a LaTeX display.
latex :: String -> DisplayData
latex = DisplayData MimeLatex . T.pack

-- | Generate a Javascript display.
javascript :: String -> DisplayData
javascript = DisplayData MimeJavascript . T.pack

-- | Generate a PNG display of the given width and height. Data must be provided in a Base64 encoded
-- manner, suitable for embedding into HTML. The @base64@ function may be used to encode data into
-- this format.
png :: Width -> Height -> Base64 -> DisplayData
png width height = DisplayData (MimePng width height)

-- | Generate a JPG display of the given width and height. Data must be provided in a Base64 encoded
-- manner, suitable for embedding into HTML. The @base64@ function may be used to encode data into
-- this format.
jpg :: Width -> Height -> Base64 -> DisplayData
jpg width height = DisplayData (MimeJpg width height)

-- | Convert from a string into base 64 encoded data.
encode64 :: String -> Base64
encode64 str = base64 $ CBS.pack str

-- | Convert from a ByteString into base 64 encoded data.
base64 :: ByteString -> Base64
base64 = E.decodeUtf8 . Base64.encode

-- | For internal use within IHaskell. Serialize displays to a ByteString.
serializeDisplay :: Display -> ByteString
serializeDisplay = Serialize.encode

-- | Items written to this chan will be included in the output sent to the frontend (ultimately the
-- browser), the next time IHaskell has an item to display.
{-# NOINLINE displayChan #-}
displayChan :: TChan Display
displayChan = unsafePerformIO newTChanIO

-- | Take everything that was put into the 'displayChan' at that point out, and make a 'Display' out
-- of it.
displayFromChanEncoded :: IO ByteString
displayFromChanEncoded =
  Serialize.encode <$> Just . many <$> unfoldM (atomically $ tryReadTChan displayChan)

-- | Write to the display channel. The contents will be displayed in the notebook once the current
-- execution call ends.
printDisplay :: IHaskellDisplay a => a -> IO ()
printDisplay disp = display disp >>= atomically . writeTChan displayChan

-- | Convenience function for client libraries. Switch to a temporary directory so that any files we
-- create aren't visible. On Unix, this is usually /tmp.
switchToTmpDir = void (try switchDir :: IO (Either SomeException ()))
  where
    switchDir =
      getTemporaryDirectory >>=
      setCurrentDirectory
