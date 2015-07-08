{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module IHaskell.Display.Widgets.Common where

import Data.Aeson
import Data.Text (pack, Text)

import Data.Singletons.TH

-- Widget properties
singletons [d|
  data Field = ModelModule
             | ModelName
             | ViewModule
             | ViewName
             | MsgThrottle
             | Version
             | OnDisplayed
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
             deriving (Eq, Ord, Show)
             |]

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



