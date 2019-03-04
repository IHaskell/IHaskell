{-# language NoImplicitPrelude, DoAndIfThenElse, OverloadedStrings, ExtendedDefaultRules #-}
module IHaskell.Publish
  ( publishResult
  ) where

import           IHaskellPrelude

import qualified Data.Text as T
<<<<<<< HEAD
import qualified Data.Text.Encoding as E
=======
import qualified Data.Time.Clock.System as Time
>>>>>>> Make unique labels using a timestamp for svg elements.

import           IHaskell.Display
import           IHaskell.Types
import           IHaskell.CSS (ihaskellCSS)

-- | Publish evaluation results, ignore any CommMsgs. This function can be used to create a function
-- of type (EvaluationResult -> IO ()), which can be used to publish results to the frontend. The
-- resultant function shares some state between different calls by storing it inside the MVars
-- passed while creating it using this function. Pager output is accumulated in the MVar passed for
-- this purpose if a pager is being used (indicated by an argument), and sent to the frontend
-- otherwise.
publishResult :: (Message -> IO ()) -- ^ A function to send messages
              -> MessageHeader      -- ^ Message header to use for reply
              -> MVar [Display]     -- ^ A MVar to use for displays
              -> MVar Bool          -- ^ A mutable boolean to decide whether the output need to be cleared and
                                    -- redrawn
              -> MVar [DisplayData] -- ^ A MVar to use for storing pager output
              -> Bool               -- ^ Whether to use the pager
              -> EvaluationResult   -- ^ The evaluation result
              -> IO ()
publishResult send replyHeader displayed updateNeeded poutput upager result = do
  let final =
        case result of
          IntermediateResult{} -> False
          FinalResult{}        -> True
      outs = evaluationOutputs result

  -- Get time to send to output for unique labels.
  uniqueLabel <- getUniqueLabel

  -- If necessary, clear all previous output and redraw.
  clear <- readMVar updateNeeded
  when clear $ do
    clearOutput
    disps <- readMVar displayed
    mapM_ (sendOutput uniqueLabel) $ reverse disps

  -- Draw this message.
  sendOutput uniqueLabel outs

  -- If this is the final message, add it to the list of completed messages. If it isn't, make sure we
  -- clear it later by marking update needed as true.
  modifyMVar_ updateNeeded (const $ return $ not final)
  when final $ do
    modifyMVar_ displayed (return . (outs :))

    -- If this has some pager output, store it for later.
    case result of
      IntermediateResult _ -> pure ()
      FinalResult _ pager _ ->
        unless (null pager) $
          if upager
            then modifyMVar_ poutput (return . (++ pager))
            else sendOutput uniqueLabel $ Display pager

  where
    clearOutput = do
      hdr <- dupHeader replyHeader ClearOutputMessage
      send $ ClearOutput hdr True

    sendOutput uniqueLabel (ManyDisplay manyOuts) =
      mapM_ (sendOutput uniqueLabel) manyOuts
    sendOutput uniqueLabel (Display outs) = do
      hdr <- dupHeader replyHeader DisplayDataMessage
<<<<<<< HEAD
      send $ PublishDisplayData hdr (map (convertSvgToHtml . prependCss) outs) Nothing

    convertSvgToHtml (DisplayData MimeSvg s) = html $ makeSvgImg $ base64 $ E.encodeUtf8 s
    convertSvgToHtml x = x

    makeSvgImg :: Base64 -> String
    makeSvgImg base64data = T.unpack $ "<img src=\"data:image/svg+xml;base64," <>
                                       base64data <>
                                       "\"/>"
=======
      send $ PublishDisplayData hdr (map (makeUnique uniqueLabel . prependCss) outs) Nothing
>>>>>>> Make unique labels using a timestamp for svg elements.

    prependCss (DisplayData MimeHtml h) =
      DisplayData MimeHtml $ mconcat ["<style>", T.pack ihaskellCSS, "</style>", h]
    prependCss x = x

    makeUnique l (DisplayData MimeSvg s) =
      DisplayData MimeSvg
        . T.replace "glyph" ("glyph-" <> l)
        . T.replace "\"clip" ("\"clip-" <> l)
        . T.replace "#clip" ("#clip-" <> l)
        . T.replace "\"image" ("\"image-" <> l)
        . T.replace "#image" ("#image-" <> l)
        . T.replace "linearGradient id=\"linear" ("linearGradient id=\"linear-" <> l)
        . T.replace "#linear" ("#linear-" <> l)
        $ s
    makeUnique _ x = x

    getUniqueLabel =
      fmap (\(Time.MkSystemTime s p) -> T.pack (show s) <> T.pack (show p))
        Time.getSystemTime
