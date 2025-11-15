{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

-- There are lots of pattern synpnyms, and little would be gained by adding
-- the type signatures.
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module IHaskell.Display.Widgets.Common (
  module IHaskell.Display.Widgets.Common,
  module IHaskell.Display.Widgets.Singletons
  ) where

import           Data.Aeson
import           Data.Aeson.Types (emptyObject)
import           Data.Text (pack, Text)
import           Data.Typeable (Typeable)

import           IHaskell.Display (IHaskellWidget)
import           IHaskell.Eval.Widgets (widgetSendClose)

import qualified IHaskell.Display.Widgets.Singletons as S
import IHaskell.Display.Widgets.Singletons

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key    as Key
#else
import           Data.HashMap.Strict as HM
#endif

-- | Close a widget's comm
closeWidget :: IHaskellWidget w => w -> IO ()
closeWidget w = widgetSendClose w emptyObject

-- | Transforms the Integer to a String of pixels
newtype PixCount = PixCount Integer
  deriving (Num, Ord, Eq, Enum, Typeable)

instance ToJSON PixCount where
  toJSON (PixCount x) = toJSON . pack $ show x ++ "px"

-- | Font style values
data FontStyleValue = NormalFont
                    | ItalicFont
                    | ObliqueFont
                    | InitialFont
                    | InheritFont
                    | DefaultFont

instance ToJSON FontStyleValue where
  toJSON NormalFont = "normal"
  toJSON ItalicFont = "italic"
  toJSON ObliqueFont = "oblique"
  toJSON InitialFont = "initial"
  toJSON InheritFont = "inherit"
  toJSON DefaultFont = ""

-- | Font weight values
data FontWeightValue = NormalWeight
                     | BoldWeight
                     | BolderWeight
                     | LighterWeight
                     | InheritWeight
                     | InitialWeight
                     | DefaultWeight

instance ToJSON FontWeightValue where
  toJSON NormalWeight = "normal"
  toJSON BoldWeight = "bold"
  toJSON BolderWeight = "bolder"
  toJSON LighterWeight = "lighter"
  toJSON InheritWeight = "inherit"
  toJSON InitialWeight = "initial"
  toJSON DefaultWeight = ""

-- | Pre-defined button styles
data ButtonStyleValue = PrimaryButton
                      | SuccessButton
                      | InfoButton
                      | WarningButton
                      | DangerButton
                      | DefaultButton

instance ToJSON ButtonStyleValue where
  toJSON PrimaryButton = "primary"
  toJSON SuccessButton = "success"
  toJSON InfoButton = "info"
  toJSON WarningButton = "warning"
  toJSON DangerButton = "danger"
  toJSON DefaultButton = ""

-- | Pre-defined bar styles
data BarStyleValue = SuccessBar
                   | InfoBar
                   | WarningBar
                   | DangerBar
                   | DefaultBar

instance ToJSON BarStyleValue where
  toJSON SuccessBar = "success"
  toJSON InfoBar = "info"
  toJSON WarningBar = "warning"
  toJSON DangerBar = "danger"
  toJSON DefaultBar = ""

-- | Audio formats for AudioWidget
data AudioFormatValue = MP3
                      | OGG
                      | WAV
                      | AURL
  deriving (Eq, Typeable)

instance Show AudioFormatValue where
  show MP3 = "mp3"
  show OGG = "ogg"
  show WAV = "wav"
  show AURL = "url"

instance ToJSON AudioFormatValue where
  toJSON = toJSON . pack . show

-- | Image formats for ImageWidget
data ImageFormatValue = PNG
                      | SVG
                      | JPG
                      | IURL
  deriving (Eq, Typeable)

-- | Image formats for ImageWidget
instance Show ImageFormatValue where
  show PNG = "png"
  show SVG = "svg"
  show JPG = "jpg"
  show IURL = "url"

instance ToJSON ImageFormatValue where
  toJSON = toJSON . pack . show

-- | Video formats for VideoWidget
data VideoFormatValue = MP4
                      | WEBM
                      | VURL
  deriving (Eq, Typeable)

instance Show VideoFormatValue where
  show MP4 = "mp4"
  show WEBM = "webm"
  show VURL = "url"

instance ToJSON VideoFormatValue where
  toJSON = toJSON . pack . show

-- | Orientation values.
data OrientationValue = HorizontalOrientation
                      | VerticalOrientation

instance ToJSON OrientationValue where
  toJSON HorizontalOrientation = "horizontal"
  toJSON VerticalOrientation = "vertical"

-- | Predefined styles for box widgets
data BoxStyleValue = SuccessBox
                   | InfoBox
                   | WarningBox
                   | DangerBox
                   | DefaultBox

instance ToJSON BoxStyleValue where
  toJSON SuccessBox = "success"
  toJSON InfoBox = "info"
  toJSON WarningBox = "warning"
  toJSON DangerBox = "danger"
  toJSON DefaultBox = ""

-- Could use 'lens-aeson' here but this is easier to read.
-- | Makes a lookup on a value given a path of strings to follow
nestedObjectLookup :: Value -> [Text] -> Maybe Value
nestedObjectLookup val [] = Just val
nestedObjectLookup val (x:xs) =
  case val of
#if MIN_VERSION_aeson(2,0,0)
    Object o -> (`nestedObjectLookup` xs) =<< KeyMap.lookup (Key.fromText x) o
#else
    Object o -> (`nestedObjectLookup` xs) =<< HM.lookup x o
#endif
    _ -> Nothing
