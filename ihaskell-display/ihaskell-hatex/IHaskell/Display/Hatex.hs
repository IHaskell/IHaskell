{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Provides 'IHaskellDisplay' instances for 'LaTeX' and 'LaTeXT'.
module IHaskell.Display.Hatex () where

import           IHaskell.Display
import           Text.LaTeX
import qualified Data.Text as T

instance IHaskellDisplay LaTeX where
  display = display . IHaskell.Display.latex . T.unpack . render

instance (a ~ (), IO ~ io) => IHaskellDisplay (LaTeXT io a) where
  display ma = display =<< execLaTeXT ma
