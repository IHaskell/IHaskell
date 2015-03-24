{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# LANGUAGE TupleSections, TemplateHaskell #-}

module IHaskell.Display.Rlangqq (
    module RlangQQ,
    rDisp,
    rDisplayAll,
    rOutputParsed,
    rOutput,
    getPlotNames,
    getCaptions,
    ) where

import           RlangQQ
import           RlangQQ.ParseKnitted

import           System.Directory
import           System.FilePath
import           Data.Maybe
import           Data.List
import           Text.Read
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char
import qualified Data.ByteString.Base64 as Base64
import           IHaskell.Display
import           IHaskell.Display.Blaze () -- to confirm it's installed
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as H
import           Data.Char
import           Control.Monad
import           Data.Ord
import           Data.List.Split
import           Text.XFormat.Show hiding ((<>))
import           Control.Applicative
import           Control.Concurrent
import           Data.Monoid
import           Data.Typeable

import           Control.Concurrent.STM
import           Language.Haskell.TH.Quote

-- | same as 'RlangQQ.r', but displays plots at the end too
rDisp = QuasiQuoter { quoteExp = \s -> [|do
                                           result <- $(quoteExp r s)
                                           p <- rDisplayAll
                                           printDisplay p
                                           return result|] }

rOutput :: IO [Int]
rOutput = do
  fs <- mapMaybe (readMaybe <=< stripPrefix "raw" <=< stripSuffix ".md")
        <$> getDirectoryContents "Rtmp"
  fs' <- forM fs $ \f -> (,f) <$> getModificationTime (showf ("Rtmp/raw" % Int % ".md") f)
  return $ map snd $ sortBy (flip (comparing fst)) fs'

-- | like 'stripPrefix' except on the end
stripSuffix :: String -> String -> Maybe String
stripSuffix s x = fmap reverse $ stripPrefix (reverse s) $ reverse x

rOutputParsed :: IO [KnitInteraction]
rOutputParsed = do
  ns <- rOutput
  case ns of
    []  -> return []
    n:_ -> parseKnitted <$> readFile (showf ("Rtmp/raw" % Int % ".md") n)

getPlotNames :: IO [String]
getPlotNames = do
  interactions <- rOutputParsed
  return [p | KnitInteraction _ is <- interactions
            , KnitImage _ p <- is]

getCaptions :: IO [String]
getCaptions = do
  interactions <- rOutputParsed
  return
    [c | KnitInteraction _ is <- interactions
       , KnitImage c _ <- is
       , not (isBoringCaption c)]

-- | true when the caption name looks like one knitr will automatically generate
isBoringCaption :: String -> Bool
isBoringCaption s = maybe False (all isDigit) (stripPrefix "plot of chunk unnamed-chunk-" s)

rDisplayAll :: IO Display
rDisplayAll = do
  ns <- rOutputParsed
  imgs <- sequence [displayInteraction o | KnitInteraction _ os <- ns
                                         , o <- os]
  display (mconcat imgs)

displayInteraction :: KnitOutput -> IO Display
displayInteraction (KnitPrint c) = display (plain c)
displayInteraction (KnitWarning c) = display (plain c)
displayInteraction (KnitError c) = display (plain c)
displayInteraction (KnitAsIs c) = display (plain c)
displayInteraction (KnitImage cap img) = do
  let caption
        | isBoringCaption cap = mempty
        | otherwise = H.p (H.toMarkup cap)
  encoded <- Base64.encode <$> B.readFile img
  display $ H.img H.! H.src
                        (H.unsafeByteStringValue
                           -- assumes you use the default device which is png
                           (Char.pack "data:image/png;base64," <> encoded))
            <> caption
