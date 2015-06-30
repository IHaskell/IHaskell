{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}

module IHaskell.Display.Widgets.Common (
    -- * Convenience types
    PosInt(..),
    -- * Convenience functions (for internal use)
    update,
    modify,
    str,
    ) where

import           Data.Aeson hiding (Success)
import           Data.Aeson.Types (Pair)
import qualified Data.Text as T
import           IHaskell.Display
import           IHaskell.Eval.Widgets
import           IHaskell.IPython.Message.UUID

-- | A wrapper around Int. 'toJSON' gives the no. for positive numbers, and empty string otherwise
newtype PosInt = PosInt { unwrap :: Int }

instance ToJSON PosInt where
  toJSON (PosInt n)
    | n > 0 = toJSON $ str $ show n
    | otherwise = toJSON $ str $ ""

-- | Send an update msg for a widget, with custom json. Make it easy to update fragments of the
-- state, by accepting a Pair instead of a Value.
update :: IHaskellWidget a => a -> [Pair] -> IO ()
update widget = widgetSendUpdate widget . toJSON . object

-- | Modify attributes of a widget, stored inside it as IORefs
modify :: IHaskellWidget a => a -> (a -> IORef b) -> b -> IO ()
modify widget attr newval = writeIORef (attr widget) newval
