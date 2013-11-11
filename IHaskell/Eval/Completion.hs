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
module IHaskell.Eval.Completion (makeCompletions) where

import Prelude
import Data.List (find, isPrefixOf, nub)
import qualified GHC
import Outputable (showPpr)
import Data.Char
import Data.ByteString.UTF8 hiding (drop)
import Data.List.Split
import Data.List.Split.Internals
import Data.Maybe

import IHaskell.Types

import Control.Applicative ((<$>))

makeCompletions :: GHC.GhcMonad m => MessageHeader -> Message -> m Message
makeCompletions replyHeader (CompleteRequest _ _ line pos) = do
    names <- GHC.getRdrNamesInScope
    flags <- GHC.getProgramDynFlags

    let maybeCand = getWordAt (toString line) pos
        options =
          case maybeCand of
            Nothing -> []
            Just candidate -> nub $ filter (candidate `isPrefixOf`) $ map (showPpr flags) names
        matched_text = fromString $ fromMaybe "" maybeCand

    return $ CompleteReply replyHeader (map fromString options) matched_text line True


-- | Get the word under a given cursor location.
getWordAt :: String -> Int -> Maybe String
getWordAt xs n = map fst <$> find (elem n .  map snd) (split splitter $ zip xs [1 .. ])
  where 
    splitter = defaultSplitter {
      -- Split using only the characters, which are the first elements of
      -- the (char, index) tuple
      delimiter = Delimiter [isDelim . fst],
      -- Condense multiple delimiters into one
      condensePolicy = Condense
    }

    isDelim char =
      case drop (max 0 (n - 1)) xs of
        x:_ -> (char `elem` neverIdent) || if isSymbol x
           then isAlpha char
           else isSymbol char
        _ -> char `elem` neverIdent

    -- These are never part of an identifier, except for the dot.
    -- Qualified names are tricky!
    neverIdent = " \t(),{}[]\\'\"`."
