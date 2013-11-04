{-# LANGUAGE PatternGuards #-}
{- | Description : generates tab-completion options

 very approximate completion. Seems to generate what is required by
 <http://ipython.org/ipython-doc/dev/development/messaging.html#complete>,
 but for whatever reason nothing gets added when the liftIO below prints
 stuff like:

 > {"status":"ok","text":"import Data hea","matches":["head"]}

 When the cursor is after the hea, and you press tab.
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

makeCompletions
  :: GHC.GhcMonad m => MessageHeader -> Message -> m Message
makeCompletions replyHeader (CompleteRequest hdr code line pos) = do

    ns <- GHC.getRdrNamesInScope
    fs <- GHC.getProgramDynFlags

    let candidate = getWordAt (toString line) pos
        opts | Just cand <- candidate = filter (cand `isPrefixOf`) $ map (showPpr fs) ns
             | otherwise = []
        matched_text = fromString $ maybe "" id candidate

    let reply = CompleteReply replyHeader (map fromString opts) matched_text line True
    liftIO (L.putStrLn $ encode $ toJSON reply)
    return reply


-- there are better ways to accomplish this
getWordAt :: String -> Int -> Maybe String
getWordAt xs n =
        fmap (map fst) $
        find (any (== n) .  map snd) $
        split (defaultSplitter{
                        delimiter = Delimiter [ (==) ' ' . fst ],
                        condensePolicy = Condense })
                (zip xs [1 .. ])

