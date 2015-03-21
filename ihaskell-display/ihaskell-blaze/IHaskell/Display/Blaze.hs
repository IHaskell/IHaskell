{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.Blaze () where

import           IHaskell.Display

import           Text.Printf
import           Text.Blaze.Html
import           Text.Blaze.Renderer.Pretty
import           Text.Blaze.Internal
import           Control.Monad

instance IHaskellDisplay (MarkupM a) where
  display val = return $ Display [stringDisplay, htmlDisplay]
    where
      str = renderMarkup (void val)
      stringDisplay = plain str
      htmlDisplay = html str
