-- | This module provides parsers for message bodies for
-- @IHaskell.Message.Parser@, and should not be used elsewhere. The
-- 'header' field on all these messages is initialized to 'undefined' and
-- is inserted afterwards.
module IHaskell.Message.BodyParser (
  kernelInfoRequestParser,
  executeRequestParser
)  where

import BasicPrelude
import Data.Aeson
import Data.Aeson.Types (parse)

import IHaskell.Types

-- | Parse a kernel info request.
-- A kernel info request has no auxiliary information, so ignore the body.
kernelInfoRequestParser :: LByteString -> Message
kernelInfoRequestParser _ = KernelInfoRequest { header = undefined }

-- | Parse an execute request.
-- Fields used are:
--  1. "code": the code to execute.
--  2. "silent": whether to execute silently.
--  3. "store_history": whether to include this in history.
--  4. "allow_stdin": whether to allow reading from stdin for this code.
executeRequestParser :: LByteString -> Message
executeRequestParser content = 
  let parser obj = do
        code <- obj .: "code"
        silent <- obj .: "silent"
        storeHistory <- obj .: "store_history"
        allowStdin <- obj .: "allow_stdin"

        return (code, silent, storeHistory, allowStdin)
      Just decoded = decode content
      Success (code, silent, storeHistory, allowStdin) = parse parser decoded in
    ExecuteRequest {
      header = undefined,
      getCode = code,
      getSilent = silent,
      getAllowStdin = allowStdin,
      getStoreHistory = storeHistory,
      getUserVariables = [],
      getUserExpressions = []
    }
