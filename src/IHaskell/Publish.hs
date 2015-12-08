module IHaskell.Publish (publishResult) where

import           IHaskellPrelude

import qualified Data.Text as T
import qualified Data.Text.Encoding as E

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
publishResult send replyHeader displayed updateNeeded pagerOutput usePager result = do
  let final =
        case result of
          IntermediateResult{} -> False
          FinalResult{}        -> True
      outs = outputs result

  -- If necessary, clear all previous output and redraw.
  clear <- readMVar updateNeeded
  when clear $ do
    clearOutput
    disps <- readMVar displayed
    mapM_ sendOutput $ reverse disps

  -- Draw this message.
  sendOutput outs

  -- If this is the final message, add it to the list of completed messages. If it isn't, make sure we
  -- clear it later by marking update needed as true.
  modifyMVar_ updateNeeded (const $ return $ not final)
  when final $ do
    modifyMVar_ displayed (return . (outs :))

    -- If this has some pager output, store it for later.
    let pager = pagerOut result
    unless (null pager) $
      if usePager
        then modifyMVar_ pagerOutput (return . (++ pager))
        else sendOutput $ Display pager

  where
    clearOutput = do
      header <- dupHeader replyHeader ClearOutputMessage
      send $ ClearOutput header True

    sendOutput (ManyDisplay manyOuts) = mapM_ sendOutput manyOuts
    sendOutput (Display outs) = do
      header <- dupHeader replyHeader DisplayDataMessage
      send $ PublishDisplayData header "haskell" $ map (convertSvgToHtml . prependCss) outs

    convertSvgToHtml (DisplayData MimeSvg svg) = html $ makeSvgImg $ base64 $ E.encodeUtf8 svg
    convertSvgToHtml x = x

    makeSvgImg :: Base64 -> String
    makeSvgImg base64data = T.unpack $ "<img src=\"data:image/svg+xml;base64," <>
                                       base64data <>
                                       "\"/>"

    prependCss (DisplayData MimeHtml html) =
      DisplayData MimeHtml $ mconcat ["<style>", T.pack ihaskellCSS, "</style>", html]
    prependCss x = x
