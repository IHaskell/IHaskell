{-# LANGUAGE OverloadedStrings #-}

module IHaskell.Eval.Evaluate.HTML (htmlify) where

import           Data.Function ((&))
import qualified Data.List as L
import           Data.Maybe
import           Data.Text as T hiding (concat)
import           GHC.SyntaxHighlighter (tokenizeHaskell)
import qualified GHC.SyntaxHighlighter as SH
import           IHaskell.Display (html')
import           IHaskell.IPython.Types (DisplayData)


htmlify :: Maybe Text -> Text -> String -> DisplayData
htmlify wrapClass classPrefix str1 = html' Nothing outerDiv
  where
    outerDiv = T.unpack ("<div class=\"" <> T.intercalate " " classNames <> "\">" <> spans <> "</div>")

    classNames = "code" : catMaybes [wrapClass]

    spans :: Text
    spans = T.intercalate "\n" (fmap renderLine (getLines tokensAndTexts))

    renderLine xs = mconcat ["<span class=\"" <> classPrefix <> tokenToClassName token <> "\">" <> escapeHtml text <> "</span>"
                            | (token, text) <- xs]

    tokensAndTexts = fromMaybe [] (tokenizeHaskell (T.pack str1))

    escapeHtml text = text
                    & T.replace "\n" "<br />"

    getLines :: [(SH.Token, Text)] -> [[(SH.Token, Text)]]
    getLines [] = []
    getLines xs = (curLine <> [spaceBoundary]) : getLines (L.tail rest)
      where (curLine, rest) = L.span (/= spaceBoundary) xs

    spaceBoundary = (SH.SpaceTok, "\n")

tokenToClassName :: SH.Token -> Text
tokenToClassName SH.KeywordTok     = "keyword"
tokenToClassName SH.PragmaTok      = "meta"
tokenToClassName SH.SymbolTok      = "atom"
tokenToClassName SH.VariableTok    = "variable"
tokenToClassName SH.ConstructorTok = "variable-2"
tokenToClassName SH.OperatorTok    = "operator"
tokenToClassName SH.CharTok        = "char"
tokenToClassName SH.StringTok      = "string"
tokenToClassName SH.IntegerTok     = "number"
tokenToClassName SH.RationalTok    = "number"
tokenToClassName SH.CommentTok     = "comment"
tokenToClassName SH.SpaceTok       = "space"
tokenToClassName SH.OtherTok       = "builtin"
