{-# LANGUAGE ViewPatterns, TemplateHaskell  #-}
-- | Description : a quasiquote for pattern matching on hashmaps
module IHaskell.Convert.HMQQ (q) where
import Language.Haskell.TH.Quote
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.Meta
import Data.Aeson
import Control.Monad
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T

-- | so you can write
-- > f [p| "abc de" : v1, abc : v2 |] = g v1 v2
fromK (VarP n) = lift (show n)
fromK (LitP x) = litE x
fromK x = error ("invalid key:"++show x)

-- | incomplete conversion of haskell literals into literals that stand for
-- aeson's Value
fixP s@(LitP (StringL {})) = conP 'String [return s]
fixP x = return x

extractJSONs :: Pat -> Maybe (ExpQ, PatQ)
extractJSONs (ListP ps) = Just $
    let (e,p) = unzip [ (\m -> [| M.lookup $(fromK k `sigE` [t| T.Text |]) $m |],
                            conP 'Just [fixP p]) 
                          | ~ (UInfixP k cons p) <- ps,
                            ~ True <- [cons == '(:)] ]
    in ([| \m -> $(tupE (map ($ [| m |]) e)) |], tupP p)
extractJSONs _ = Nothing

q = QuasiQuoter
    { quotePat = \s -> case parsePat ("[" ++ s ++ "]") of
            Right (extractJSONs -> Just (e,p)) -> viewP e p
            x -> fail (show x),
      quoteExp = error "q",
      quoteType = error "q",
      quoteDec = error "q"
    }
