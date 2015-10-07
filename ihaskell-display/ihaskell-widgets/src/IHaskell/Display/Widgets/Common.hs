{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}

module IHaskell.Display.Widgets.Common where

import           Data.Aeson
import           Data.Aeson.Types (emptyObject)
import           Data.Text (pack, Text)
import           Data.Typeable (Typeable)

import           IHaskell.Display (IHaskellWidget)
import           IHaskell.Eval.Widgets (widgetSendClose)

import qualified IHaskell.Display.Widgets.Singletons as S

pattern ViewModule = S.SViewModule
pattern ViewName = S.SViewName
pattern ModelModule = S.SModelModule
pattern ModelName = S.SModelName
pattern MsgThrottle = S.SMsgThrottle
pattern Version = S.SVersion
pattern DisplayHandler = S.SDisplayHandler
pattern Visible = S.SVisible
pattern CSS = S.SCSS
pattern DOMClasses = S.SDOMClasses
pattern Width = S.SWidth
pattern Height = S.SHeight
pattern Padding = S.SPadding
pattern Margin = S.SMargin
pattern Color = S.SColor
pattern BackgroundColor = S.SBackgroundColor
pattern BorderColor = S.SBorderColor
pattern BorderWidth = S.SBorderWidth
pattern BorderRadius = S.SBorderRadius
pattern BorderStyle = S.SBorderStyle
pattern FontStyle = S.SFontStyle
pattern FontWeight = S.SFontWeight
pattern FontSize = S.SFontSize
pattern FontFamily = S.SFontFamily
pattern Description = S.SDescription
pattern ClickHandler = S.SClickHandler
pattern SubmitHandler = S.SSubmitHandler
pattern Disabled = S.SDisabled
pattern StringValue = S.SStringValue
pattern Placeholder = S.SPlaceholder
pattern Tooltip = S.STooltip
pattern Icon = S.SIcon
pattern ButtonStyle = S.SButtonStyle
pattern B64Value = S.SB64Value
pattern ImageFormat = S.SImageFormat
pattern BoolValue = S.SBoolValue
pattern Options = S.SOptions
pattern SelectedLabel = S.SSelectedLabel
pattern SelectedValue = S.SSelectedValue
pattern SelectionHandler = S.SSelectionHandler
pattern Tooltips = S.STooltips
pattern Icons = S.SIcons
pattern SelectedLabels = S.SSelectedLabels
pattern SelectedValues = S.SSelectedValues
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
pattern ShowRange = S.SShowRange
pattern ReadOut = S.SReadOut
pattern SliderColor = S.SSliderColor
pattern BarStyle = S.SBarStyle
pattern ChangeHandler = S.SChangeHandler
pattern Children = S.SChildren
pattern OverflowX = S.SOverflowX
pattern OverflowY = S.SOverflowY
pattern BoxStyle = S.SBoxStyle
pattern Flex = S.SFlex
pattern Pack = S.SPack
pattern Align = S.SAlign
pattern Titles = S.STitles
pattern SelectedIndex = S.SSelectedIndex
pattern ReadOutMsg = S.SReadOutMsg
pattern Child = S.SChild
pattern Selector = S.SSelector

-- | Close a widget's comm
closeWidget :: IHaskellWidget w => w -> IO ()
closeWidget w = widgetSendClose w emptyObject

newtype PixCount = PixCount Integer
  deriving (Num, Ord, Eq, Enum, Typeable)

instance ToJSON PixCount where
  toJSON (PixCount x) = toJSON . pack $ show x ++ "px"

-- | Pre-defined border styles
data BorderStyleValue = NoBorder
                      | HiddenBorder
                      | DottedBorder
                      | DashedBorder
                      | SolidBorder
                      | DoubleBorder
                      | GrooveBorder
                      | RidgeBorder
                      | InsetBorder
                      | OutsetBorder
                      | InitialBorder
                      | InheritBorder
                      | DefaultBorder

instance ToJSON BorderStyleValue where
  toJSON NoBorder = "none"
  toJSON HiddenBorder = "hidden"
  toJSON DottedBorder = "dotted"
  toJSON DashedBorder = "dashed"
  toJSON SolidBorder = "solid"
  toJSON DoubleBorder = "double"
  toJSON GrooveBorder = "groove"
  toJSON RidgeBorder = "ridge"
  toJSON InsetBorder = "inset"
  toJSON OutsetBorder = "outset"
  toJSON InitialBorder = "initial"
  toJSON InheritBorder = "inherit"
  toJSON DefaultBorder = ""

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

-- | Image formats for ImageWidget
data ImageFormatValue = PNG
                      | SVG
                      | JPG
  deriving (Eq, Typeable)

instance Show ImageFormatValue where
  show PNG = "png"
  show SVG = "svg"
  show JPG = "jpg"

instance ToJSON ImageFormatValue where
  toJSON = toJSON . pack . show

-- | Options for selection widgets.
data SelectionOptions = OptionLabels [Text]
                      | OptionDict [(Text, Text)]

-- | Orientation values.
data OrientationValue = HorizontalOrientation
                      | VerticalOrientation

instance ToJSON OrientationValue where
  toJSON HorizontalOrientation = "horizontal"
  toJSON VerticalOrientation = "vertical"

data OverflowValue = VisibleOverflow
                   | HiddenOverflow
                   | ScrollOverflow
                   | AutoOverflow
                   | InitialOverflow
                   | InheritOverflow
                   | DefaultOverflow

instance ToJSON OverflowValue where
  toJSON VisibleOverflow = "visible"
  toJSON HiddenOverflow = "hidden"
  toJSON ScrollOverflow = "scroll"
  toJSON AutoOverflow = "auto"
  toJSON InitialOverflow = "initial"
  toJSON InheritOverflow = "inherit"
  toJSON DefaultOverflow = ""

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
