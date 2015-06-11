{-# LANGUAGE TypeSynonymInstances, QuasiQuotes, FlexibleInstances, OverloadedStrings #-}

module IHaskell.Display.Parsec () where

import           System.Random
import           Data.String.Here
import           Data.HashMap.Strict as Map
import           Control.Applicative ((<$>))

import qualified Data.Text as T
import           Data.Text (Text)

import           Text.Parsec (parse, sourceLine, sourceColumn)
import           Text.Parsec.String (Parser)
import           Text.Parsec.Error (errorPos, ParseError)

import           Data.Aeson

import           IHaskell.Display

instance Show a => IHaskellDisplay (Parser a) where
  display renderable = return $ many [Display [javascript js], Display [html dom]]
    where
      dom = [hereFile|widget.html|]
      js = [hereFile|widget.js|]

-- | Text to parse.
data ParseText = ParseText String

instance FromJSON ParseText where
  parseJSON (Object v) = ParseText <$> v .: "text"
  parseJSON _ = fail "Expecting object"

-- | Output of parsing.
instance Show a => ToJSON (Either ParseError a) where
  toJSON (Left err) = object
                        [ "status" .= ("error" :: String)
                        , "line" .= sourceLine (errorPos err)
                        , "col" .= sourceColumn (errorPos err)
                        , "msg" .= show err
                        ]
  toJSON (Right result) = object ["status" .= ("success" :: String), "result" .= show result]

instance Show a => IHaskellWidget (Parser a) where
  -- Name for this widget.
  targetName _ = "parsec"
  -- When we rece
  comm widget (Object dict) publisher = do
    let key = "text" :: Text
        Just (String text) = Map.lookup key dict
        result = parse widget "<interactive>" $ T.unpack text
    publisher $ toJSON result
