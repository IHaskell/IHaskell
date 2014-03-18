{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, QuasiQuotes, FlexibleInstances, OverloadedStrings #-}
module IHaskell.Display.Parsec () where

import ClassyPrelude hiding (fromList)
import System.Random
import Data.String.Here
import Data.HashMap.Strict as Map

import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.String
import Text.Parsec.Error

import Data.Aeson

import IHaskell.Display

instance Show a => IHaskellDisplay (Parser a) where
  display renderable = return $ Display [html dom]
    where 
      dom = 
        [i|
        <script src="/static/components/codemirror/addon/lint/lint.js" charset="utf-8"></script>
        <form><textarea id="parsec-editor">Insert parser text here...</textarea></form>
        <pre id="parsec-output"></pre>
        |]

-- | Text to parse.
data ParseText = ParseText String

instance FromJSON ParseText where
  parseJSON (Object v) = ParseText <$> v .: "text"
  parseJSON _          = fail "Expecting object"

-- | Output of parsing.
instance Show a => ToJSON (Either ParseError a) where
  toJSON (Left err) = object [
                        "status" .= ("error" :: String),
                        "line" .= sourceLine (errorPos err),
                        "col" .= sourceColumn (errorPos err),
                        "msg" .= show err
                      ]
  toJSON (Right result) = object [
                               "status" .= ("success" :: String),
                               "result" .= show result
                            ]

instance Show a => IHaskellWidget (Parser a) where
  -- Name for this widget.
  targetName _ = "parsec"

  -- When we rece
  comm widget (Object dict) publisher = do
    let key = "text" :: Text
        Just (String text) = Map.lookup key dict
        result = parse widget "<interactive>" $ unpack text
    publisher $ toJSON result

  -- We have no resources to close.
  close widget value = return ()
