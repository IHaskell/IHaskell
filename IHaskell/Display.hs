{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module IHaskell.Display (
  IHaskellDisplay(..),
  MimeType(..),
  DisplayData(..),
  ) where

import IHaskell.Types

-- | A class for displayable Haskell types.
class IHaskellDisplay a where
  display :: a -> [DisplayData]
