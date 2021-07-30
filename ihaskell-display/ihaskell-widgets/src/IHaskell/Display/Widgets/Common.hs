{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- There are lots of pattern synpnyms, and little would be gained by adding
-- the type signatures.
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module IHaskell.Display.Widgets.Common where

import           Data.Aeson
import           Data.Aeson.Types (emptyObject)
import           Data.HashMap.Strict as HM
import           Data.Text (pack, Text)
import           Data.Typeable (Typeable)

import           IHaskell.Display (IHaskellWidget)
import           IHaskell.Eval.Widgets (widgetSendClose)

import qualified IHaskell.Display.Widgets.Singletons as S

pattern ViewModule = S.SViewModule
pattern ViewModuleVersion = S.SViewModuleVersion
pattern ViewName = S.SViewName
pattern ModelModule = S.SModelModule
pattern ModelModuleVersion = S.SModelModuleVersion
pattern ModelName = S.SModelName
pattern DisplayHandler = S.SDisplayHandler
pattern DOMClasses = S.SDOMClasses
pattern Layout = S.SLayout
pattern Width = S.SWidth
pattern Height = S.SHeight
pattern Description = S.SDescription
pattern ClickHandler = S.SClickHandler
pattern SubmitHandler = S.SSubmitHandler
pattern Disabled = S.SDisabled
pattern StringValue = S.SStringValue
pattern Placeholder = S.SPlaceholder
pattern Tooltip = S.STooltip
pattern Icon = S.SIcon
pattern ButtonStyle = S.SButtonStyle
pattern BSValue = S.SBSValue
pattern ImageFormat = S.SImageFormat
pattern BoolValue = S.SBoolValue
pattern OptionsLabels = S.SOptionsLabels
pattern OptionalIndex = S.SOptionalIndex
pattern Index = S.SIndex
pattern SelectionHandler = S.SSelectionHandler
pattern Tooltips = S.STooltips
pattern Icons = S.SIcons
pattern Indices = S.SIndices
pattern IntValue = S.SIntValue
pattern StepInt = S.SStepInt
pattern MaxInt = S.SMaxInt
pattern MinInt = S.SMinInt
pattern IntPairValue = S.SIntPairValue
pattern LowerInt = S.SLowerInt
pattern UpperInt = S.SUpperInt
pattern FloatValue = S.SFloatValue
pattern StepFloat = S.SStepFloat
pattern MaxFloat = S.SMaxFloat
pattern MinFloat = S.SMinFloat
pattern FloatPairValue = S.SFloatPairValue
pattern LowerFloat = S.SLowerFloat
pattern UpperFloat = S.SUpperFloat
pattern Orientation = S.SOrientation
pattern BaseFloat = S.SBaseFloat
pattern ReadOut = S.SReadOut
pattern ReadOutFormat = S.SReadOutFormat
pattern BarStyle = S.SBarStyle
pattern ChangeHandler = S.SChangeHandler
pattern Children = S.SChildren
pattern BoxStyle = S.SBoxStyle
pattern Pack = S.SPack
pattern Align = S.SAlign
pattern Titles = S.STitles
pattern SelectedIndex = S.SSelectedIndex
pattern ReadOutMsg = S.SReadOutMsg
pattern Indent = S.SIndent
pattern Child = S.SChild
pattern Selector = S.SSelector
pattern ContinuousUpdate = S.SContinuousUpdate
pattern Tabbable = S.STabbable
pattern Rows = S.SRows
pattern AudioFormat = S.SAudioFormat
pattern VideoFormat = S.SVideoFormat
pattern AutoPlay = S.SAutoPlay
pattern Loop = S.SLoop
pattern Controls = S.SControls
pattern Options = S.SOptions
pattern EnsureOption = S.SEnsureOption
pattern Playing = S.SPlaying
pattern Repeat = S.SRepeat
pattern Interval = S.SInterval
pattern ShowRepeat = S.SShowRepeat
pattern Concise = S.SConcise
pattern DateValue = S.SDateValue
pattern Pressed = S.SPressed
pattern Name = S.SName
pattern Mapping = S.SMapping
pattern Connected = S.SConnected
pattern Timestamp = S.STimestamp
pattern Buttons = S.SButtons
pattern Axes = S.SAxes
pattern ButtonColor = S.SButtonColor
pattern FontWeight = S.SFontWeight
pattern DescriptionWidth = S.SDescriptionWidth

pattern StyleButton = S.SStyleButton
pattern StyleDescription = S.SStyleDescription
pattern StyleProgress = S.SStyleProgress
pattern StyleSlider = S.SStyleSlider
pattern StyleToggleButton = S.SStyleToggleButton

-- | Close a widget's comm
closeWidget :: IHaskellWidget w => w -> IO ()
closeWidget w = widgetSendClose w emptyObject

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

data LocationValue = StartLocation
                   | CenterLocation
                   | EndLocation
                   | BaselineLocation
                   | StretchLocation

instance ToJSON LocationValue where
  toJSON StartLocation = "start"
  toJSON CenterLocation = "center"
  toJSON EndLocation = "end"
  toJSON BaselineLocation = "baseline"
  toJSON StretchLocation = "stretch"

-- Could use 'lens-aeson' here but this is easier to read.
nestedObjectLookup :: Value -> [Text] -> Maybe Value
nestedObjectLookup val [] = Just val
nestedObjectLookup val (x:xs) =
  case val of
    Object o -> (`nestedObjectLookup` xs) =<< HM.lookup x o
    _ -> Nothing
