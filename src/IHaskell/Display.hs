{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, FlexibleInstances #-}
module IHaskell.Display (
  -- * How to get IHaskell to display different types
  IHaskellDisplay(..),
  -- $displayDynCommentary
  --
  -- $displayDynExBad
  displayDyn,
  -- $displayChanCommentary
  displayChan,

  -- * Ways to create 'Display'
  Display(..),
  DisplayData(..),

  plain, html, png, jpg, svg, latex,
  Width, Height, Base64,
  encode64, base64,

  -- * for internal use
  serializeDisplay,
  displayClass,
  displayFromDyn,
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

-- | see 'displayDyn'
displayFromDyn :: Typeable a => a -> IO (Maybe Display)
displayFromDyn x = do
     fs <- readMVar displayDyn 
     let go [] = return Nothing
         go (f:fs) = do
            fx <- f (toDyn x)
            case fx of
                First Nothing -> go fs
                First (Just y) -> return (Just y)
     go fs


-- IHaskell tries to apply functions from this list when displaying things:
-- the first function that produces a 'Display' (rather than @First Nothing@)
-- is used.
{-# NOINLINE displayDyn #-}
displayDyn :: MVar [Dynamic -> IO (First Display)]
displayDyn = unsafePerformIO (newMVar [])

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
    Just . many <$> unfoldM (atomically (tryReadTChan displayChan))

-- | This is unfoldM from monad-loops. It repeatedly runs an IO action
-- until it return Nothing, and puts all the Justs in a list.
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
serializeDisplay :: Maybe Display -> ByteString
serializeDisplay = Serialize.encode

-- | This variation of 'display' that always has a 'Just' is used to
-- simplify code in "IHaskell.Eval.Evaluate", which needs to handle
-- the case when 'displayFromDyn' does not find a function to display.
displayClass :: IHaskellDisplay a => a -> IO (Maybe Display) 
displayClass = fmap Just . display

{- $displayDynCommentary

There is some overlap between what you can do with 'displayDyn' and what
you can do by writing an instance of 'IHaskellDisplay'. The following is a
comparison of the two approaches.

Suppose that we want to display images generated in the IHaskell notebook,
and that they are defined:

> newtype Image = Image ByteString deriving Typeable

One function that can make a 'Display' out of 'Image' is:

> displayImage :: Int -> Int -> Image -> IO Display
> displayImage w h (Image x) = display (png w h (base64 x))

It is straightforward to define an IHaskellDisplay instance. This one
assumes you always want a fixed image size:

> instance IHaskellDisplay DynamicImage where
>  display = displayImage 640 480

The equivalent results can be achieved with the 'displayDyn' MVar too. 

> -- first a utility function that applies displayImage iff the
> -- the Dynamic contains an Image
> handleImage :: Int -> Int -> Dynamic -> IO (First Display)
> handleImage w h = fmap First . traverse (displayImage w h) . fromDynamic
>
> -- then in the notebook you can add a
> modifyMVar_ displayDyn (return . (handleImage 640 480 :))

Then later on after making some pictures, you can add another
function that will be used instead:

> modifyMVar_ displayDyn (return . (handleImage 1280 800 :))

To approach displayDyn version, you would have to write all
IHaskellDisplay instances using a global variable containing
the function that is actually used. A minor issue is that you cannot remove
instances, but you can remove elements from the list referenced by
'displayDyn'
-}

-- this one is separate because of a haddock bug
-- http://trac.haskell.org/haddock/ticket/282

-- $displayDynExBad
--
-- > {-# NOINLINE imageDisplay #-}
-- > displayImageRef :: IORef (Image -> IO Display)
-- > displayImageRef = unsafePerformIO (displayImage 640 480)
-- >
-- > instance IHaskellDisplay Image where
-- >   display img = do
-- >       fn <- readIORef displayImageRef
-- >       fn img 

{- $displayChanCommentary

In the notebook, writes to stdout are captured and 'display'ed later
on. Without 'displayChan', this functionality is available if the output is
in some format besides plain text. In other words, a function might be:

> histWithPlot :: Vector Double -> IO (Vector Int, Display)

Then in the notebook, users have to explicitly handle that argument:

> (binnedCounts, showPlot) <- histWithPlot ys
> showPlot
> .. do stuff with binnedCounts ..

If we instead define a new @histWithPlot'@ that sends the image to
'displayChan':

> histWithPlot' :: Vector Double -> IO (Vector Int)
> histWithPlot' ys = do
>     (binnedCounts, showPlot) <- histWithPlot ys
>     writeTChan displayChan showPlot
>     return binnedCounts

Then code in the notebook is \"better\"

> binnedCounts <- histWithPlot ys
> .. do stuff with binnedCounts ...

-}
