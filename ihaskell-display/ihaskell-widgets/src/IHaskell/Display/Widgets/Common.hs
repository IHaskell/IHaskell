{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IHaskell.Display.Widgets.Common where

import Data.Aeson
import Data.Text (pack, Text)

import Data.Singletons.TH

-- Widget properties
singletons [d|
  data Field = ViewModule
             | ViewName
             | MsgThrottle
             | Version
             | DisplayHandler
             | Visible
             | CSS
             | DOMClasses
             | Width
             | Height
             | Padding
             | Margin
             | Color
             | BackgroundColor
             | BorderColor
             | BorderWidth
             | BorderRadius
             | BorderStyle
             | FontStyle
             | FontWeight
             | FontSize
             | FontFamily
             | Description
             | ClickHandler
             | SubmitHandler
             | Disabled
             | StringValue
             | Placeholder
             | Tooltip
             | Icon
             | ButtonStyle
             | B64Value
             | ImageFormat
             | BoolValue
             | Options
             | SelectedLabel
             | SelectedValue
             | SelectionHandler
             | Tooltips
             | Icons
             | SelectedLabels
             | SelectedValues
             | IntValue
             | StepInt
             | MaxInt
             | MinInt
             | IntPairValue
             | LowerInt
             | UpperInt
             | FloatValue
             | StepFloat
             | MaxFloat
             | MinFloat
             | FloatPairValue
             | LowerFloat
             | UpperFloat
             | Orientation
             | ShowRange
             | ReadOut
             | SliderColor
             | BarStyle
             | ChangeHandler
             | Children
             | OverflowX
             | OverflowY
             | BoxStyle
             deriving (Eq, Ord, Show)
             |]

newtype StrInt = StrInt Integer deriving (Num, Ord, Eq, Enum)

instance ToJSON StrInt where
  toJSON (StrInt x) = toJSON . pack $ show x

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
  deriving Eq

instance Show ImageFormatValue where
  show PNG = "png"
  show SVG = "svg"
  show JPG = "jpg"

instance ToJSON ImageFormatValue where
  toJSON = toJSON . pack . show

-- | Options for selection widgets.
data SelectionOptions = OptionLabels [Text] | OptionDict [(Text, Text)]

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
