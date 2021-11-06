{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}

-- There are lots of pattern synpnyms, and little would be gained by adding
-- the type signatures.
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module IHaskell.Display.Widgets.Common where

import           Data.Aeson
import           Data.Aeson.Types (emptyObject)
import           Data.Text (pack, Text)
import           Data.Typeable (Typeable)

import           IHaskell.Display (IHaskellWidget)
import           IHaskell.Eval.Widgets (widgetSendClose)

import qualified IHaskell.Display.Widgets.Singletons as S

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key    as Key
#else
import           Data.HashMap.Strict as HM
#endif

-- | The view module string
pattern ViewModule = S.SViewModule
-- | The view module version
pattern ViewModuleVersion = S.SViewModuleVersion
-- | The view name
pattern ViewName = S.SViewName
-- | The model module string
pattern ModelModule = S.SModelModule
-- | The model module version
pattern ModelModuleVersion = S.SModelModuleVersion
-- | The model name
pattern ModelName = S.SModelName
-- | A method to be called on display
pattern DisplayHandler = S.SDisplayHandler
-- | CSS classes applied to widget DOM element
pattern DOMClasses = S.SDOMClasses
-- | Reference to a Layout widget
pattern Layout = S.SLayout
-- | Width of the video/image in pixels
pattern Width = S.SWidth
-- | Height of the video/image in pixels
pattern Height = S.SHeight
-- | Description of the control
pattern Description = S.SDescription
-- | Method to be called on click
pattern ClickHandler = S.SClickHandler
-- | Method to be called on submit
pattern SubmitHandler = S.SSubmitHandler
-- | Whether the widget appears as disabled on the frontend
pattern Disabled = S.SDisabled
-- | The value of the widget, of type string
pattern StringValue = S.SStringValue
-- | Placeholder text to display if nothing has been typed yet
pattern Placeholder = S.SPlaceholder
-- | Tooltip for the description
pattern Tooltip = S.STooltip
-- | The font-awesome icon without the fa-
pattern Icon = S.SIcon
-- | Predefined styling for the button
pattern ButtonStyle = S.SButtonStyle
-- | Value of the widget of type bytestring
pattern BSValue = S.SBSValue
-- | The format of the image
pattern ImageFormat = S.SImageFormat
-- | The value of the widget of type bool
pattern BoolValue = S.SBoolValue
-- | The labels for the options
pattern OptionsLabels = S.SOptionsLabels
-- | Selected index, can be Nothing
pattern OptionalIndex = S.SOptionalIndex
-- | The index of the controller
pattern Index = S.SIndex
-- | Method to be called when something is chosen
pattern SelectionHandler = S.SSelectionHandler
-- | Tooltips for each button
pattern Tooltips = S.STooltips
-- | Icons names for each button (FontAwesome names without the fa- prefix)
pattern Icons = S.SIcons
-- | Selected indices
pattern Indices = S.SIndices
-- | The value of the widget of type int
pattern IntValue = S.SIntValue
-- | Minimum step to increment the value
pattern StepInt = S.SStepInt
-- | Max value
pattern MaxInt = S.SMaxInt
-- | Min value
pattern MinInt = S.SMinInt
-- | The value of the widget as an int pair
pattern IntPairValue = S.SIntPairValue
-- | Min value on a range widget
pattern LowerInt = S.SLowerInt
-- | Max value on a range widget
pattern UpperInt = S.SUpperInt
-- | Value of the widget (float)
pattern FloatValue = S.SFloatValue
-- | Minimum step to increment the value
pattern StepFloat = S.SStepFloat
-- | Max value
pattern MaxFloat = S.SMaxFloat
-- | Min value
pattern MinFloat = S.SMinFloat
-- | Value of the widget as a float pair
pattern FloatPairValue = S.SFloatPairValue
-- | Min value of a range widget
pattern LowerFloat = S.SLowerFloat
-- | Max value of a range widget
pattern UpperFloat = S.SUpperFloat
-- | Orientation of the widget
pattern Orientation = S.SOrientation
-- | The logarithmic base of the widget
pattern BaseFloat = S.SBaseFloat
-- | Whether to display the current value of the widget next to it
pattern ReadOut = S.SReadOut
-- | The format of the readout
pattern ReadOutFormat = S.SReadOutFormat
-- | Use a predefined styling for the bar
pattern BarStyle = S.SBarStyle
-- | A method called when the value changes in the fronted
pattern ChangeHandler = S.SChangeHandler
-- | List of widget children
pattern Children = S.SChildren
-- | Use a predefined styling for the box
pattern BoxStyle = S.SBoxStyle
-- | Titles of the pages
pattern Titles = S.STitles
-- | The index of the selected page. Is nothing if no widgets are selected.
pattern SelectedIndex = S.SSelectedIndex
-- | Message displayed when the value is false
pattern ReadOutMsg = S.SReadOutMsg
-- | Indent the control to align with other controls with a description
pattern Indent = S.SIndent
-- | Update the value as the user types. If false, update on submission.
pattern ContinuousUpdate = S.SContinuousUpdate
-- | The number of rows to display
pattern Rows = S.SRows
-- | The format of the audio
pattern AudioFormat = S.SAudioFormat
-- | The format of the image
pattern VideoFormat = S.SVideoFormat
-- | When true, the video starts on display
pattern AutoPlay = S.SAutoPlay
-- | When true, the video starts from the beginning after finishing
pattern Loop = S.SLoop
-- | Specifies that video controls should be displayed
pattern Controls = S.SControls
-- | Dropdown options for the combobox
pattern Options = S.SOptions
-- | If set, ensure the value is in options
pattern EnsureOption = S.SEnsureOption
-- | Whether the control is currently playing
pattern Playing = S.SPlaying
-- | Whether the control will repeat in a continuous loop
pattern Repeat = S.SRepeat
-- | The maximum interval for the play control
pattern Interval = S.SInterval
-- | Show the repeat toggle button on the widget
pattern ShowRepeat = S.SShowRepeat
-- | Display the short version of the selector
pattern Concise = S.SConcise
-- | The value of the widget in date format
pattern DateValue = S.SDateValue
-- | Whether the button is pressed
pattern Pressed = S.SPressed
-- | The name of the controller
pattern Name = S.SName
-- | The name of the control mapping
pattern Mapping = S.SMapping
-- | Whether the gamepad is connected
pattern Connected = S.SConnected
-- | The last time the data from this gamepad was updated
pattern Timestamp = S.STimestamp
-- | The button widgets on the gamepad
pattern Buttons = S.SButtons
-- | The axes on the gamepad
pattern Axes = S.SAxes
-- | Color of the button
pattern ButtonColor = S.SButtonColor
-- | The font weight of the text
pattern FontWeight = S.SFontWeight
-- | Width of the description to the side of the control
pattern DescriptionWidth = S.SDescriptionWidth
-- | Color of the progress bar
pattern BarColor = S.SBarColor
-- | Color of the slider handle
pattern HandleColor = S.SHandleColor
-- | The width of each button
pattern ButtonWidth = S.SButtonWidth
-- | The target (widget,field) pair
pattern Target = S.STarget
-- | The source (widget,field) pair
pattern Source = S.SSource
-- | Parent message id of messages to capture
pattern MsgID = S.SMsgID
-- | The output messages synced from the frontend
pattern Outputs = S.SOutputs
-- | Reference to a Style widget with styling customizations
pattern Style = S.SStyle

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
