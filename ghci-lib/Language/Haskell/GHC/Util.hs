module Language.Haskell.GHC.Util where

import GhcMonad
import GHC
import DynFlags
import Outputable
import Packages
import Module
import qualified Pretty
import FastString

doc :: GhcMonad m => SDoc -> m String
doc sdoc = do
  flags <- getSessionDynFlags
  unqual <- getPrintUnqual
  let style = mkUserStyle unqual AllTheWay
  let cols = pprCols flags
      d = runSDoc sdoc (initSDocContext flags style)
  return $ Pretty.fullRender Pretty.PageMode cols 1.5 string_txt "" d
  where
    string_txt :: Pretty.TextDetails -> String -> String
    string_txt (Pretty.Chr c)   s  = c:s
    string_txt (Pretty.Str s1)  s2 = s1 ++ s2
    string_txt (Pretty.PStr s1) s2 = unpackFS s1 ++ s2
    string_txt (Pretty.LStr s1 _) s2 = unpackLitString s1 ++ s2
