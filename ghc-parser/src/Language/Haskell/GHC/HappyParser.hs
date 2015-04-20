{-# LANGUAGE CPP #-}

module Language.Haskell.GHC.HappyParser
 (
#if __GLASGOW_HASKELL__ >= 760 && __GLASGOW_HASKELL__ < 780
    module Language.Haskell.GHC.HappyParser_760
#elif __GLASGOW_HASKELL__ >= 780 && __GLASGOW_HASKELL__ < 783
    module Language.Haskell.GHC.HappyParser_782
#elif __GLASGOW_HASKELL__ < 710
    module Language.Haskell.GHC.HappyParser_783
#else
    module Language.Haskell.GHC.HappyParser_710
#endif
 )

import Language.Haskell.GHC.HappyParser_760
import Language.Haskell.GHC.HappyParser_782
import Language.Haskell.GHC.HappyParser_783
import Language.Haskell.GHC.HappyParser_710
