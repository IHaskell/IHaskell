{-# LANGUAGE PatternGuards #-}
{- | Description : generates tab-completion options

  context-insensitive completion for what is probably
  the identifier under the cursor.
  
  [@Known issues@]

  > import Data.Lef<tab>
  > System.IO.h<tab>
  > Just.he<tab>

  The first should not complete to Left. The second should only
  include things like System.IO.hPutStrLn, not head. Qualified
  names should not be confused by the third option.

-}
module IHaskell.Completion (makeCompletions) where

import Prelude
import Data.List
import IHaskell.Types
import GhcMonad(liftIO, GhcMonad)
import qualified GHC
import Outputable (showPpr)
import Data.Char
import Data.ByteString.UTF8
import Data.List.Split
import Data.List.Split.Internals
import Data.Aeson
import IHaskell.Message.Writer
import qualified Data.ByteString.Lazy as L
import Data.Maybe

makeCompletions
  :: GHC.GhcMonad m => MessageHeader -> Message -> m Message
makeCompletions replyHeader (CompleteRequest hdr code line pos) = do

    ns <- GHC.getRdrNamesInScope
    fs <- GHC.getProgramDynFlags

    let candidate = getWordAt (toString line) pos
        opts | Just cand <- candidate = filter (cand `isPrefixOf`) $ map (showPpr fs) ns
             | otherwise = []
        matched_text = fromString $ fromMaybe "" candidate

    return $ CompleteReply replyHeader (map fromString opts) matched_text line True


-- maybe there are better ways to be sure we're getting only
-- the whole word under the cursor...
getWordAt :: String -> Int -> Maybe String
getWordAt xs n =
        fmap (map fst) $
        find (any (== n) .  map snd) $
        split (defaultSplitter{
                        delimiter = Delimiter [ isDelim . fst ],
                        condensePolicy = Condense })
                (zip xs [1 .. ])

    where isDelim | x:_ <- Data.List.drop (max 0 (n-1)) xs = \s ->
            (s `elem` neverIdent)
            || if isSymbol x then isAlpha s
                            else isSymbol s
            | otherwise = \s -> s `elem` neverIdent
          -- these (and others?) are never part of an identifier
          -- except for the dot (qualified names are tricky)
          neverIdent = " \t(),{}[]\\'\"`."
