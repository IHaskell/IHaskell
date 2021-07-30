{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module houses all the type-trickery needed to make widgets happen.
--
-- All widgets have a corresponding 'WidgetType', and some fields/attributes/properties as defined
-- by the 'WidgetFields' type-family.
--
-- Each widget field corresponds to a concrete haskell type, as given by the 'FieldType'
-- type-family.
--
-- Vinyl records are used to wrap together widget fields into a single 'WidgetState'.
--
-- Singletons are used as a way to represent the promoted types of kind Field. For example:
--
-- @
-- SViewName :: SField ViewName
-- @
--
-- This allows the user to pass the type 'ViewName' without using Data.Proxy. In essence, a
-- singleton is the only inhabitant (other than bottom) of a promoted type. Single element set/type
-- == singleton.
--
-- It also allows the record to wrap values of properties with information about their Field type. A
-- vinyl record is represented as @Rec f ts@, which means that a record is a list of @f x@, where
-- @x@ is a type present in the type-level list @ts@. Thus a 'WidgetState' is essentially a list of
-- field properties wrapped together with the corresponding promoted Field type. See ('=::') for
-- more.
--
-- The properties function can be used to view all the @Field@s associated with a widget object.
--
-- Attributes are represented by the @Attr@ data type, which holds the value of a field, along with
-- the actual @Field@ object and a function to verify validity of changes to the value.
--
-- The IPython widgets expect state updates of the form {"property": value}, where an empty string
-- for numeric values is ignored by the frontend and the default value is used instead. Some numbers
-- need to be sent as numbers (represented by @Integer@), whereas some (css lengths) need to be sent
-- as Strings (@PixCount@).
--
-- Child widgets are expected to be sent as strings of the form "IPY_MODEL_<uuid>", where @<uuid>@
-- represents the uuid of the widget's comm.
--
-- To know more about the IPython messaging specification (as implemented in this package) take a
-- look at the supplied MsgSpec.md.
--
-- Widgets are not able to do console input, the reason for that can be found in the messaging
-- specification.
module IHaskell.Display.Widgets.Types where

import           Control.Monad (unless, join, when, void,mzero)
import           Control.Applicative ((<$>))
import qualified Control.Exception as Ex
import           Data.Typeable (Typeable, TypeRep, typeOf)
import           Data.IORef (IORef, readIORef, modifyIORef)
import           Data.Text (Text, pack)
import           System.IO.Error
import           System.Posix.IO
import           Text.Printf (printf)

import           Data.Aeson hiding (pairs)
import           Data.Aeson.Types (Pair)
import           Data.ByteString (ByteString)
import           Data.Int (Int16)
#if MIN_VERSION_vinyl(0,9,0)
import           Data.Vinyl (Rec(..), Dict(..))
import           Data.Vinyl.Recursive ((<+>), recordToList, reifyConstraint, rmap)
#else
import           Data.Vinyl (Rec(..), (<+>), recordToList, reifyConstraint, rmap, Dict(..))
#endif
import           Data.Vinyl.Functor (Compose(..), Const(..))
import           Data.Vinyl.Lens (rget, rput, type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

#if MIN_VERSION_singletons(3,0,0)
import           Data.List.Singletons
#elif MIN_VERSION_singletons(2,4,0)
import           Data.Singletons.Prelude.List
#else
import           Data.Singletons.Prelude ((:++))
#endif

#if MIN_VERSION_singletons(3,0,0)
import           Data.Singletons.Base.TH
#else
import           Data.Singletons.TH
#endif

import           Data.Text.Lazy (unpack)
import           Data.Text.Lazy.Encoding

import           GHC.IO.Exception

import           IHaskell.Eval.Widgets (widgetSendUpdate, widgetSendView)
import           IHaskell.Display (Base64, IHaskellWidget(..), IHaskellDisplay(..), Display(..), widgetdisplay, base64)
import           IHaskell.IPython.Message.UUID

import           IHaskell.Display.Widgets.Singletons (Field, SField)
import qualified IHaskell.Display.Widgets.Singletons as S
import           IHaskell.Display.Widgets.Common

#if MIN_VERSION_singletons(2,4,0)
-- Versions of the "singletons" package are tightly tied to the GHC version.
-- Singletons versions 2.3.* and earlier used the type level operator ':++'
-- for appending type level lists while 2.4.* and latter use the normal value
-- level list append operator '++'.
-- To maintain compatibility across GHC versions we keep using the ':++'
-- operator for now.
type (a :++ b) = a ++ b
#endif

-- Classes from IPython's widget hierarchy. Defined as such to reduce code duplication.
type CoreWidgetClass = ['S.ViewModule, 'S.ViewModuleVersion, 'S.ModelModule, 'S.ModelModuleVersion ]

type DOMWidgetClass = ['S.ModelName, 'S.ViewName, 'S.DOMClasses, 'S.Tabbable, 'S.Tooltip, 'S.Layout, 'S.DisplayHandler]

type StyleWidgetClass = ['S.ModelName, 'S.ViewName] :++ CoreWidgetClass

type DescriptionWidgetClass = CoreWidgetClass :++ DOMWidgetClass :++ '[ 'S.Description ]

type StringClass = DescriptionWidgetClass :++ ['S.StringValue, 'S.Placeholder]

type TextClass = StringClass :++ [ 'S.Disabled, 'S.ContinuousUpdate, 'S.SubmitHandler, 'S.ChangeHandler]

type BoolClass = DescriptionWidgetClass :++ ['S.BoolValue, 'S.Disabled, 'S.ChangeHandler]

type SelectionClass = DescriptionWidgetClass :++ ['S.OptionsLabels, 'S.OptionalIndex, 'S.Disabled, 'S.SelectionHandler]

type SelectionNonemptyClass = DescriptionWidgetClass :++ ['S.OptionsLabels, 'S.Index, 'S.Disabled, 'S.SelectionHandler]

type MultipleSelectionClass = DescriptionWidgetClass :++ ['S.OptionsLabels, 'S.Indices, 'S.Disabled, 'S.SelectionHandler]

type IntClass = DescriptionWidgetClass :++ [ 'S.IntValue, 'S.ChangeHandler ]

type BoundedIntClass = IntClass :++ ['S.MaxInt, 'S.MinInt]

type IntRangeClass = IntClass :++ ['S.IntPairValue, 'S.LowerInt, 'S.UpperInt]

type BoundedIntRangeClass = IntRangeClass :++ ['S.MaxInt, 'S.MinInt]

type FloatClass = DescriptionWidgetClass :++ [ 'S.FloatValue, 'S.ChangeHandler ]

type BoundedFloatClass = FloatClass :++ ['S.MinFloat, 'S.MaxFloat]

type BoundedLogFloatClass = FloatClass :++ [ 'S.MinFloat, 'S.MaxFloat, 'S.BaseFloat ]

type FloatRangeClass = FloatClass :++ '[ 'S.FloatPairValue ]

type BoundedFloatRangeClass = FloatRangeClass :++ ['S.StepFloat, 'S.MinFloat, 'S.MaxFloat]

type BoxClass = CoreWidgetClass :++ DOMWidgetClass :++ ['S.Children, 'S.BoxStyle]

type SelectionContainerClass = BoxClass :++ ['S.Titles, 'S.SelectedIndex, 'S.ChangeHandler]

type MediaClass = CoreWidgetClass :++ DOMWidgetClass :++ '[ 'S.BSValue ]

-- Types associated with Fields.
type family FieldType (f :: Field) :: *

type instance FieldType 'S.ViewModule = Text
type instance FieldType 'S.ViewModuleVersion = Text
type instance FieldType 'S.ViewName = Text
type instance FieldType 'S.ModelModule = Text
type instance FieldType 'S.ModelModuleVersion = Text
type instance FieldType 'S.ModelName = Text
type instance FieldType 'S.Layout = IPythonWidget 'LayoutType
type instance FieldType 'S.DisplayHandler = IO ()
type instance FieldType 'S.DOMClasses = [Text]
type instance FieldType 'S.Width = PixCount
type instance FieldType 'S.Height = PixCount
type instance FieldType 'S.Description = Text
type instance FieldType 'S.ClickHandler = IO ()
type instance FieldType 'S.SubmitHandler = IO ()
type instance FieldType 'S.Disabled = Bool
type instance FieldType 'S.StringValue = Text
type instance FieldType 'S.Placeholder = Text
type instance FieldType 'S.Tooltip = Maybe Text
type instance FieldType 'S.Icon = Text
type instance FieldType 'S.ButtonStyle = ButtonStyleValue
type instance FieldType 'S.BSValue = ByteString
type instance FieldType 'S.ImageFormat = ImageFormatValue
type instance FieldType 'S.BoolValue = Bool
type instance FieldType 'S.OptionsLabels = [Text]
type instance FieldType 'S.Index = Integer
type instance FieldType 'S.OptionalIndex = Maybe Integer
type instance FieldType 'S.SelectionHandler = IO ()
type instance FieldType 'S.Tooltips = [Text]
type instance FieldType 'S.Icons = [Text]
type instance FieldType 'S.Indices = [Integer]
type instance FieldType 'S.IntValue = Integer
type instance FieldType 'S.StepInt = Maybe Integer
type instance FieldType 'S.MinInt = Integer
type instance FieldType 'S.MaxInt = Integer
type instance FieldType 'S.LowerInt = Integer
type instance FieldType 'S.UpperInt = Integer
type instance FieldType 'S.IntPairValue = (Integer, Integer)
type instance FieldType 'S.Orientation = OrientationValue
type instance FieldType 'S.BaseFloat = Double
type instance FieldType 'S.ReadOut = Bool
type instance FieldType 'S.ReadOutFormat = Text
type instance FieldType 'S.BarStyle = BarStyleValue
type instance FieldType 'S.FloatValue = Double
type instance FieldType 'S.StepFloat = Maybe Double
type instance FieldType 'S.MinFloat = Double
type instance FieldType 'S.MaxFloat = Double
type instance FieldType 'S.LowerFloat = Double
type instance FieldType 'S.UpperFloat = Double
type instance FieldType 'S.FloatPairValue = (Double, Double)
type instance FieldType 'S.ChangeHandler = IO ()
type instance FieldType 'S.Children = [ChildWidget]
type instance FieldType 'S.BoxStyle = BoxStyleValue
type instance FieldType 'S.Pack = LocationValue
type instance FieldType 'S.Align = LocationValue
type instance FieldType 'S.Titles = [Text]
type instance FieldType 'S.SelectedIndex = Maybe Integer
type instance FieldType 'S.ReadOutMsg = Text
type instance FieldType 'S.Indent = Bool
type instance FieldType 'S.Child = Maybe ChildWidget
type instance FieldType 'S.Selector = Text
type instance FieldType 'S.ContinuousUpdate = Bool
type instance FieldType 'S.Tabbable = Maybe Bool
type instance FieldType 'S.Rows = Maybe Integer
type instance FieldType 'S.AudioFormat = AudioFormatValue
type instance FieldType 'S.VideoFormat = VideoFormatValue
type instance FieldType 'S.AutoPlay = Bool
type instance FieldType 'S.Loop = Bool
type instance FieldType 'S.Controls = Bool
type instance FieldType 'S.Options = [Text]
type instance FieldType 'S.EnsureOption = Bool
type instance FieldType 'S.Playing = Bool
type instance FieldType 'S.Repeat = Bool
type instance FieldType 'S.Interval = Integer
type instance FieldType 'S.ShowRepeat = Bool
type instance FieldType 'S.Concise = Bool
type instance FieldType 'S.DateValue = Date
type instance FieldType 'S.Pressed = Bool
type instance FieldType 'S.Name = Text
type instance FieldType 'S.Mapping = Text
type instance FieldType 'S.Connected = Bool
type instance FieldType 'S.Timestamp = Double
type instance FieldType 'S.Buttons = [IPythonWidget 'ControllerButtonType]
type instance FieldType 'S.Axes = [IPythonWidget 'ControllerAxisType]
type instance FieldType 'S.ButtonColor = Maybe String
type instance FieldType 'S.FontWeight = FontWeightValue
type instance FieldType 'S.StyleButton = IPythonWidget 'ButtonStyleType
type instance FieldType 'S.StyleDescription = IPythonWidget 'DescriptionStyleType
type instance FieldType 'S.StyleProgress = IPythonWidget 'ProgressStyleType
type instance FieldType 'S.StyleSlider = IPythonWidget 'SliderStyleType
type instance FieldType 'S.StyleToggleButton = IPythonWidget 'ToggleButtonsStyleType

-- | Can be used to put different widgets in a list. Useful for dealing with children widgets.
data ChildWidget = forall w. RecAll Attr (WidgetFields w) ToPairs => ChildWidget (IPythonWidget w)

instance ToJSON (IPythonWidget w) where
  toJSON x = toJSON . pack $ "IPY_MODEL_" ++ uuidToString (uuid x)

instance ToJSON ChildWidget where
  toJSON (ChildWidget x) = toJSON x

-- Will use a custom class rather than a newtype wrapper with an orphan instance. The main issue is
-- the need of a Bounded instance for Float / Double.
class CustomBounded a where
  lowerBound :: a
  upperBound :: a

-- Set according to what IPython widgets use
instance CustomBounded PixCount where
  lowerBound = - fromIntegral (maxBound :: Int16)
  upperBound = fromIntegral (maxBound :: Int16)

instance CustomBounded Integer where
  lowerBound = - fromIntegral (maxBound :: Int16)
  upperBound = fromIntegral (maxBound :: Int16)

instance CustomBounded Double where
  lowerBound = - fromIntegral (maxBound :: Int16)
  upperBound = fromIntegral (maxBound :: Int16)

-- Different types of widgets. Every widget in IPython has a corresponding WidgetType
data WidgetType = ButtonType
                | ColorPickerType
                | DatePickerType
                | AudioType
                | ImageType
                | VideoType
                | OutputType
                | ComboboxType
                | HTMLType
                | HTMLMathType
                | LabelType
                | PasswordType
                | TextType
                | TextAreaType
                | CheckBoxType
                | ToggleButtonType
                | ValidType
                | DropdownType
                | RadioButtonsType
                | SelectType
                | SelectionSliderType
                | SelectionRangeSliderType
                | ToggleButtonsType
                | SelectMultipleType
                | IntTextType
                | BoundedIntTextType
                | IntSliderType
                | PlayType
                | IntProgressType
                | IntRangeSliderType
                | FloatTextType
                | BoundedFloatTextType
                | FloatSliderType
                | FloatLogSliderType
                | FloatProgressType
                | FloatRangeSliderType
                | BoxType
                | GridBoxType
                | HBoxType
                | VBoxType
                | AccordionType
                | TabType
                | StackedType
                | ControllerButtonType
                | ControllerAxisType
                | ControllerType
                | LayoutType
                | ButtonStyleType
                | DescriptionStyleType
                | ProgressStyleType
                | SliderStyleType
                | ToggleButtonsStyleType

-- Fields associated with a widget

type family WidgetFields (w :: WidgetType) :: [Field]
type instance WidgetFields 'ButtonType =
                DescriptionWidgetClass :++
                  ['S.Disabled, 'S.Icon, 'S.ButtonStyle ,'S.StyleButton,'S.ClickHandler]
type instance WidgetFields 'ColorPickerType =
                DescriptionWidgetClass :++
                  ['S.StringValue, 'S.Concise, 'S.Disabled]
type instance WidgetFields 'DatePickerType =
                DescriptionWidgetClass :++
                  ['S.DateValue, 'S.Disabled]

type instance WidgetFields 'AudioType =
                MediaClass :++ ['S.AudioFormat, 'S.AutoPlay, 'S.Loop, 'S.Controls]
type instance WidgetFields 'ImageType =
                MediaClass :++ ['S.ImageFormat, 'S.Width, 'S.Height]
type instance WidgetFields 'VideoType =
                MediaClass :++ ['S.VideoFormat, 'S.Width, 'S.Height, 'S.AutoPlay, 'S.Loop, 'S.Controls]

type instance WidgetFields 'OutputType = DOMWidgetClass
type instance WidgetFields 'HTMLType = StringClass
type instance WidgetFields 'HTMLMathType = StringClass
type instance WidgetFields 'ComboboxType = TextClass :++ [ 'S.Options, 'S.EnsureOption ]
type instance WidgetFields 'LabelType = StringClass
type instance WidgetFields 'PasswordType = TextClass
type instance WidgetFields 'TextType = TextClass

-- Type level lists with a single element need both the list and the
-- constructor ticked, and a space between the open square bracket and
-- the first constructor. See https://ghc.haskell.org/trac/ghc/ticket/15601
type instance WidgetFields 'TextAreaType =
                StringClass :++
                  [ 'S.Rows, 'S.Disabled, 'S.ContinuousUpdate, 'S.ChangeHandler]

type instance WidgetFields 'CheckBoxType = BoolClass :++ '[ 'S.Indent ]
type instance WidgetFields 'ToggleButtonType = BoolClass :++ ['S.Icon, 'S.ButtonStyle]
type instance WidgetFields 'ValidType = BoolClass :++ '[ 'S.ReadOutMsg ]
type instance WidgetFields 'DropdownType = SelectionClass
type instance WidgetFields 'RadioButtonsType = SelectionClass
type instance WidgetFields 'SelectType = SelectionClass :++ '[ 'S.Rows ]
type instance WidgetFields 'SelectionSliderType = SelectionNonemptyClass :++ '[ 'S.Orientation, 'S.ReadOut, 'S.ContinuousUpdate ]
type instance WidgetFields 'SelectionRangeSliderType = MultipleSelectionClass :++ '[ 'S.Orientation, 'S.ReadOut, 'S.ContinuousUpdate ]
type instance WidgetFields 'ToggleButtonsType =
                SelectionClass :++ ['S.Tooltips, 'S.Icons, 'S.ButtonStyle]
type instance WidgetFields 'SelectMultipleType = MultipleSelectionClass :++ '[ 'S.Rows ]
type instance WidgetFields 'IntTextType = IntClass :++ [ 'S.Disabled, 'S.ContinuousUpdate, 'S.StepInt ]
type instance WidgetFields 'BoundedIntTextType = BoundedIntClass :++ [ 'S.Disabled, 'S.ContinuousUpdate, 'S.StepInt ]
type instance WidgetFields 'IntSliderType =
                BoundedIntClass :++
                  [ 'S.StepInt, 'S.Orientation, 'S.ReadOut, 'S.ReadOutFormat, 'S.ContinuousUpdate, 'S.Disabled ]
type instance WidgetFields 'PlayType =
                BoundedIntClass :++
                  [ 'S.Playing, 'S.Repeat, 'S.Interval, 'S.StepInt, 'S.Disabled, 'S.ShowRepeat ]
type instance WidgetFields 'IntProgressType =
                BoundedIntClass :++ ['S.Orientation, 'S.BarStyle]
type instance WidgetFields 'IntRangeSliderType =
                BoundedIntRangeClass :++
                  ['S.StepInt, 'S.Orientation, 'S.ReadOut, 'S.ReadOutFormat, 'S.ContinuousUpdate, 'S.Disabled ]
type instance WidgetFields 'FloatTextType = FloatClass :++ '[ 'S.Disabled, 'S.ContinuousUpdate, 'S.StepFloat ]
type instance WidgetFields 'BoundedFloatTextType = BoundedFloatClass :++ '[ 'S.Disabled, 'S.ContinuousUpdate, 'S.StepFloat ]
type instance WidgetFields 'FloatSliderType =
                BoundedFloatClass :++
                  ['S.StepFloat, 'S.Orientation, 'S.ReadOut, 'S.ReadOutFormat, 'S.ContinuousUpdate, 'S.Disabled ]
type instance WidgetFields 'FloatLogSliderType =
                BoundedLogFloatClass :++
                  ['S.StepFloat, 'S.Orientation, 'S.ReadOut, 'S.ReadOutFormat, 'S.ContinuousUpdate, 'S.Disabled, 'S.BaseFloat]
type instance WidgetFields 'FloatProgressType =
                BoundedFloatClass :++ ['S.Orientation, 'S.BarStyle]
type instance WidgetFields 'FloatRangeSliderType =
                BoundedFloatRangeClass :++
                  ['S.StepFloat, 'S.Orientation, 'S.ReadOut, 'S.ReadOutFormat, 'S.ContinuousUpdate, 'S.Disabled ]
type instance WidgetFields 'BoxType = BoxClass
type instance WidgetFields 'GridBoxType = BoxClass
type instance WidgetFields 'HBoxType = BoxClass
type instance WidgetFields 'VBoxType = BoxClass
type instance WidgetFields 'AccordionType = SelectionContainerClass
type instance WidgetFields 'TabType = SelectionContainerClass
type instance WidgetFields 'StackedType = SelectionContainerClass
type instance WidgetFields 'ControllerType =
  CoreWidgetClass :++ DOMWidgetClass :++
    ['S.Index, 'S.Name, 'S.Mapping, 'S.Connected, 'S.Timestamp, 'S.Buttons, 'S.Axes, 'S.ChangeHandler ]
type instance WidgetFields 'ControllerAxisType = CoreWidgetClass :++ DOMWidgetClass :++ '[ 'S.FloatValue, 'S.ChangeHandler ]
type instance WidgetFields 'ControllerButtonType = CoreWidgetClass :++ DOMWidgetClass :++ [ 'S.FloatValue, 'S.Pressed, 'S.ChangeHandler ]

type instance WidgetFields 'ButtonStyleType = StyleWidgetClass :++ ['S.ButtonColor, 'S.FontWeight]

-- Wrapper around a field's value. A dummy value is sent as an empty string to the frontend.
data AttrVal a = Dummy a
               | Real a

unwrap :: AttrVal a -> a
unwrap (Dummy x) = x
unwrap (Real x) = x

-- Wrapper around a field.
data Attr (f :: Field) where
  Attr :: Typeable (FieldType f)
       => { _value :: AttrVal (FieldType f)
          , _verify :: FieldType f -> IO (FieldType f)
          , _field :: Field
          , _ro :: Bool
          } -> Attr f

getFieldType :: Attr f -> TypeRep
getFieldType Attr { _value = attrval } = typeOf $ unwrap attrval

instance ToJSON (FieldType f) => ToJSON (Attr f) where
  toJSON attr =
    case _value attr of
      Dummy _ -> object []
      Real x  -> toJSON x

-- Types that can be converted to Aeson Pairs.
class ToPairs a where
  toPairs :: a -> [Pair]

-- Attributes that aren't synced with the frontend give [] on toPairs
instance ToPairs (Attr 'S.ViewModule) where
  toPairs x = ["_view_module" .= toJSON x]

instance ToPairs (Attr 'S.ViewModuleVersion) where
  toPairs x = ["_view_module_version" .= toJSON x]

instance ToPairs (Attr 'S.ViewName) where
  toPairs x = ["_view_name" .= toJSON x]

instance ToPairs (Attr 'S.ModelModule) where
  toPairs x = ["_model_module" .= toJSON x]

instance ToPairs (Attr 'S.ModelModuleVersion) where
  toPairs x = ["_model_module_version" .= toJSON x]

instance ToPairs (Attr 'S.ModelName) where
  toPairs x = ["_model_name" .= toJSON x]

instance ToPairs (Attr 'S.DisplayHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr 'S.DOMClasses) where
  toPairs x = ["_dom_classes" .= toJSON x]

instance ToPairs (Attr 'S.Width) where
  toPairs x = ["width" .= toJSON x]

instance ToPairs (Attr 'S.Height) where
  toPairs x = ["height" .= toJSON x]

instance ToPairs (Attr 'S.Description) where
  toPairs x = ["description" .= toJSON x]

instance ToPairs (Attr 'S.ClickHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr 'S.SubmitHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr 'S.Disabled) where
  toPairs x = ["disabled" .= toJSON x]

instance ToPairs (Attr 'S.StringValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.Placeholder) where
  toPairs x = ["placeholder" .= toJSON x]

instance ToPairs (Attr 'S.Tooltip) where
  toPairs x = ["tooltip" .= toJSON x]

instance ToPairs (Attr 'S.Icon) where
  toPairs x = ["icon" .= toJSON x]

instance ToPairs (Attr 'S.ButtonStyle) where
  toPairs x = ["button_style" .= toJSON x]

instance ToJSON ByteString where
  toJSON = toJSON . base64

instance ToPairs (Attr 'S.BSValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.ImageFormat) where
  toPairs x = ["format" .= toJSON x]

instance ToPairs (Attr 'S.AudioFormat) where
  toPairs x = ["format" .= toJSON x]

instance ToPairs (Attr 'S.VideoFormat) where
  toPairs x = ["format" .= toJSON x]

instance ToPairs (Attr 'S.BoolValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.Index) where
  toPairs x = ["index" .= toJSON x]

instance ToPairs (Attr 'S.OptionalIndex) where
  toPairs x = ["index" .= toJSON x]

instance ToPairs (Attr 'S.OptionsLabels) where
  toPairs x = ["_options_labels" .= toJSON x]

instance ToPairs (Attr 'S.SelectionHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr 'S.Tooltips) where
  toPairs x = ["tooltips" .= toJSON x]

instance ToPairs (Attr 'S.Icons) where
  toPairs x = ["icons" .= toJSON x]

instance ToPairs (Attr 'S.Indices) where
  toPairs x = ["index" .= toJSON x]

instance ToPairs (Attr 'S.IntValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.StepInt) where
  toPairs x = ["step" .= toJSON x]

instance ToPairs (Attr 'S.MinInt) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr 'S.MaxInt) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr 'S.IntPairValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.LowerInt) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr 'S.UpperInt) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr 'S.FloatValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.StepFloat) where
  toPairs x = ["step" .= toJSON x]

instance ToPairs (Attr 'S.MinFloat) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr 'S.MaxFloat) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr 'S.FloatPairValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.LowerFloat) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr 'S.UpperFloat) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr 'S.Orientation) where
  toPairs x = ["orientation" .= toJSON x]

instance ToPairs (Attr 'S.BaseFloat) where
  toPairs x = ["base" .= toJSON x]

instance ToPairs (Attr 'S.ReadOut) where
  toPairs x = ["readout" .= toJSON x]

instance ToPairs (Attr 'S.ReadOutFormat) where
  toPairs x = ["readout_format" .= toJSON x]

instance ToPairs (Attr 'S.BarStyle) where
  toPairs x = ["bar_style" .= toJSON x]

instance ToPairs (Attr 'S.ChangeHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr 'S.Children) where
  toPairs x = ["children" .= toJSON x]

instance ToPairs (Attr 'S.BoxStyle) where
  toPairs x = ["box_style" .= toJSON x]

instance ToPairs (Attr 'S.Pack) where
  toPairs x = ["pack" .= toJSON x]

instance ToPairs (Attr 'S.Align) where
  toPairs x = ["align" .= toJSON x]

instance ToPairs (Attr 'S.Titles) where
  toPairs x = ["_titles" .= toJSON x]

instance ToPairs (Attr 'S.SelectedIndex) where
  toPairs x = ["selected_index" .= toJSON x]

instance ToPairs (Attr 'S.ReadOutMsg) where
  toPairs x = ["readout" .= toJSON x]

instance ToPairs (Attr 'S.Indent) where
  toPairs x = ["indent" .= toJSON x]

instance ToPairs (Attr 'S.Child) where
  toPairs x = ["child" .= toJSON x]

instance ToPairs (Attr 'S.Selector) where
  toPairs x = ["selector" .= toJSON x]

instance ToPairs (Attr 'S.ContinuousUpdate) where
  toPairs x = ["continuous_update" .= toJSON x]

instance ToPairs (Attr 'S.Tabbable) where
  toPairs x = ["tabbable" .= toJSON x]

instance ToPairs (Attr 'S.Rows) where
  toPairs x = ["rows" .= toJSON x]

instance ToPairs (Attr 'S.AutoPlay) where
  toPairs x = ["autoplay" .= toJSON x]

instance ToPairs (Attr 'S.Loop) where
  toPairs x = ["loop" .= toJSON x]

instance ToPairs (Attr 'S.Controls) where
  toPairs x = ["controls" .= toJSON x]

instance ToPairs (Attr 'S.Options) where
  toPairs x = ["options" .= toJSON x]

instance ToPairs (Attr 'S.EnsureOption) where
  toPairs x = ["ensure_option" .= toJSON x]

instance ToPairs (Attr 'S.Playing) where
  toPairs x = ["playing" .= toJSON x]

instance ToPairs (Attr 'S.Repeat) where
  toPairs x = ["repeat" .= toJSON x]

instance ToPairs (Attr 'S.Interval) where
  toPairs x = ["interval" .= toJSON x]

instance ToPairs (Attr 'S.ShowRepeat) where
  toPairs x = ["show_repeat" .= toJSON x]

instance ToPairs (Attr 'S.Concise) where
  toPairs x = ["concise" .= toJSON x]

instance ToPairs (Attr 'S.DateValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr 'S.Pressed) where
  toPairs x = ["pressed" .= toJSON x]

instance ToPairs (Attr 'S.Name) where
  toPairs x = ["name" .= toJSON x]

instance ToPairs (Attr 'S.Mapping) where
  toPairs x = ["mapping" .= toJSON x]

instance ToPairs (Attr 'S.Connected) where
  toPairs x = ["connected" .= toJSON x]

instance ToPairs (Attr 'S.Timestamp) where
  toPairs x = ["timestamp" .= toJSON x]

instance ToPairs (Attr 'S.Buttons) where
  toPairs x = ["buttons" .= toJSON x]

instance ToPairs (Attr 'S.Axes) where
  toPairs x = ["axes" .= toJSON x]

instance ToPairs (Attr 'S.Layout) where
  toPairs x = ["layout" .= toJSON x]

instance ToPairs (Attr 'S.ButtonColor) where
  toPairs x = ["button_color" .= toJSON x]

instance ToPairs (Attr 'S.FontWeight) where
  toPairs x = ["font_weight" .= toJSON x]

-- Style widgets. Same serialization, different type
instance ToPairs (Attr 'S.StyleButton) where
  toPairs x = ["style" .= toJSON x]

instance ToPairs (Attr 'S.StyleDescription) where
  toPairs x = ["style" .= toJSON x]

instance ToPairs (Attr 'S.StyleProgress) where
  toPairs x = ["style" .= toJSON x]

instance ToPairs (Attr 'S.StyleSlider) where
  toPairs x = ["style" .= toJSON x]

instance ToPairs (Attr 'S.StyleToggleButton) where
  toPairs x = ["style" .= toJSON x]


-- | Store the value for a field, as an object parametrized by the Field. No verification is done
-- for these values.
(=::) :: (SingI f, Typeable (FieldType f)) => Sing f -> FieldType f -> Attr f
s =:: x = Attr { _value = Real x, _verify = return, _field = reflect s, _ro = False }

-- | Store the value for a field, with a custom verification
(=:.) :: (SingI f, Typeable (FieldType f)) => Sing f -> (FieldType f, FieldType f -> IO (FieldType f) ) -> Attr f
s =:. (x,v) = Attr { _value = Real x, _verify = v, _field = reflect s, _ro = False }

-- | Store the value for a field, making it read only from the frontend
(=:!) :: (SingI f, Typeable (FieldType f)) => Sing f -> FieldType f -> Attr f
s =:! x = Attr { _value = Real x, _verify = return, _field = reflect s, _ro = True}

-- | If the number is in the range, return it. Otherwise raise the appropriate (over/under)flow
-- exception.
rangeCheck :: (Num a, Ord a) => (a, a) -> a -> IO a
rangeCheck (l, u) x
  | l <= x && x <= u = return x
  | l > x = Ex.throw Ex.Underflow
  | u < x = Ex.throw Ex.Overflow
  | otherwise = error "The impossible happened in IHaskell.Display.Widgets.Types.rangeCheck"

rangeSliderVerification :: [Integer] -> IO [Integer]
rangeSliderVerification xs@[a,b]
  | a <= b    = return xs
  | otherwise = Ex.throw $ Ex.AssertionFailed "The first index should be smaller than the second"
rangeSliderVerification _ = Ex.throw $ Ex.AssertionFailed "There should be two indices"

-- | Store a numeric value, with verification mechanism for its range.
ranged :: (SingI f, Num (FieldType f), Ord (FieldType f), Typeable (FieldType f))
       => Sing f -> (FieldType f, FieldType f) -> AttrVal (FieldType f) -> Attr f
ranged s range x = Attr x (rangeCheck range) (reflect s) False

-- | Store a numeric value, with the invariant that it stays non-negative. The value set is set as a
-- dummy value if it's equal to zero.
(=:+) :: (SingI f, Num (FieldType f), CustomBounded (FieldType f), Ord (FieldType f), Typeable (FieldType f))
      => Sing f -> FieldType f -> Attr f
s =:+ val = Attr
              ((if val == 0
                  then Dummy
                  else Real)
                 val)
              (rangeCheck (0, upperBound))
              (reflect s)
              False

-- | Get a field from a singleton Adapted from: http://stackoverflow.com/a/28033250/2388535
reflect :: forall (f :: Field). (SingI f) => Sing f -> Field
reflect = fromSing

-- | A record representing a Widget class from IPython from the controls modules
defaultCoreWidget :: Rec Attr CoreWidgetClass
defaultCoreWidget = (ViewModule =:! "@jupyter-widgets/controls")
                    :& (ViewModuleVersion =:! "1.4.0")
                    :& (ModelModule =:! "@jupyter-widgets/controls")
                    :& (ModelModuleVersion =:! "1.4.0")
                    :& RNil

-- | A record representing an object of the DOMWidget class from IPython
defaultDOMWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr DOMWidgetClass
defaultDOMWidget viewName modelName layout = (ModelName =:! modelName)
                                      :& (ViewName =:! viewName)
                                      :& (DOMClasses =:: [])
                                      :& (Tabbable =:: Nothing)
                                      :& (Tooltip =:: Nothing)
                                      :& (Layout =:: layout)
                                      :& (DisplayHandler =:: return ())
                                      :& RNil

-- | A record representing an object of the DescriptionWidget class from IPython
defaultDescriptionWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr DescriptionWidgetClass
defaultDescriptionWidget viewName modelName layout = defaultCoreWidget <+> defaultDOMWidget viewName modelName layout <+> descriptionAttrs
  where
    descriptionAttrs = (Description =:: "")
                       :& RNil

-- | A record representing a widget of the _String class from IPython
defaultStringWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr StringClass
defaultStringWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> strAttrs
  where
    strAttrs = (StringValue =:: "")
               :& (Placeholder =:: "")
               :& RNil

-- | A record representing a widget of the Text class from IPython
defaultTextWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr TextClass
defaultTextWidget viewName modelName layout = defaultStringWidget viewName modelName layout <+> txtAttrs
  where
    txtAttrs = (Disabled =:: False)
               :& (ContinuousUpdate =:: True)
               :& (SubmitHandler =:: return ())
               :& (ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _Bool class from IPython
defaultBoolWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoolClass
defaultBoolWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> boolAttrs
  where
    boolAttrs = (BoolValue =:: False)
                :& (Disabled =:: False)
                :& (ChangeHandler =:: return ())
                :& RNil

-- | A record representing a widget of the _Selection class from IPython
defaultSelectionWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr SelectionClass
defaultSelectionWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> selectionAttrs
  where
    selectionAttrs = (OptionsLabels =:: [])
                     :& (OptionalIndex =:: Nothing)
                     :& (Disabled =:: False)
                     :& (SelectionHandler =:: return ())
                     :& RNil

-- | A record representing a widget of the _SelectionNonempty class from IPython
defaultSelectionNonemptyWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr SelectionNonemptyClass
defaultSelectionNonemptyWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> selectionAttrs
  where
    selectionAttrs = (OptionsLabels =:: [])
                     :& (Index =:: 0)
                     :& (Disabled =:: False)
                     :& (SelectionHandler =:: return ())
                     :& RNil

-- | A record representing a widget of the _MultipleSelection class from IPython
defaultMultipleSelectionWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr MultipleSelectionClass
defaultMultipleSelectionWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> mulSelAttrs
  where
    mulSelAttrs = (OptionsLabels =:: [])
                  :& (Indices =:: [])
                  :& (Disabled =:: False)
                  :& (SelectionHandler =:: return ())
                  :& RNil

-- | A record representing a widget of the _Int class from IPython
defaultIntWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr IntClass
defaultIntWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> intAttrs
  where
    intAttrs = (IntValue =:: 0)
               :& (ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultBoundedIntWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoundedIntClass
defaultBoundedIntWidget viewName modelName layout = defaultIntWidget viewName modelName layout <+> boundedIntAttrs
  where
    boundedIntAttrs = (MaxInt =:: 100)
                      :& (MinInt =:: 0)
                      :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultIntRangeWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr IntRangeClass
defaultIntRangeWidget viewName modelName layout = defaultIntWidget viewName modelName layout <+> rangeAttrs
  where
    rangeAttrs = (IntPairValue =:: (25, 75))
                 :& (LowerInt =:: 0)
                 :& (UpperInt =:: 100)
                 :& RNil

-- | A record representing a widget of the _BoundedIntRange class from IPython
defaultBoundedIntRangeWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoundedIntRangeClass
defaultBoundedIntRangeWidget viewName modelName layout = defaultIntRangeWidget viewName modelName layout <+> boundedIntRangeAttrs
  where
    boundedIntRangeAttrs = (MaxInt =:: 100)
                           :& (MinInt =:: 0)
                           :& RNil

-- | A record representing a widget of the _Float class from IPython
defaultFloatWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr FloatClass
defaultFloatWidget viewName modelName layout = defaultDescriptionWidget viewName modelName layout <+> floatAttrs
  where
    floatAttrs = (FloatValue =:: 0.0)
               :& (ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultBoundedFloatWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoundedFloatClass
defaultBoundedFloatWidget viewName modelName layout = defaultFloatWidget viewName modelName layout <+> boundedFloatAttrs
  where
    boundedFloatAttrs = (MinFloat =:: 0)
                        :& (MaxFloat =:: 100)
                        :& RNil

-- | A record representing a widget of the _BoundedLogFloat class from IPython
defaultBoundedLogFloatWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoundedLogFloatClass
defaultBoundedLogFloatWidget viewName modelName layout = floatAttrs <+> boundedLogFloatAttrs
  where
    floatAttrs = rput (FloatValue =:: 1.0) $ defaultFloatWidget viewName modelName layout
    boundedLogFloatAttrs = (MinFloat =:: 0.0)
                           :& (MaxFloat =:: 4.0)
                           :& (BaseFloat =:: 10.0)
                           :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultFloatRangeWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr FloatRangeClass
defaultFloatRangeWidget viewName modelName layout = defaultFloatWidget viewName modelName layout <+> rangeAttrs
  where
    rangeAttrs = (FloatPairValue =:: (0.0, 1.0))
                 :& RNil

-- | A record representing a widget of the _BoundedFloatRange class from IPython
defaultBoundedFloatRangeWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoundedFloatRangeClass
defaultBoundedFloatRangeWidget viewName modelName layout = defaultFloatRangeWidget viewName modelName layout <+> boundedFloatRangeAttrs
  where
    boundedFloatRangeAttrs = (StepFloat =:: Just 1)
                             :& (MinFloat =:: 0)
                             :& (MaxFloat =:: 100)
                             :& RNil

-- | A record representing a widget of the _Box class from IPython
defaultBoxWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr BoxClass
defaultBoxWidget viewName modelName layout = defaultCoreWidget <+> defaultDOMWidget viewName modelName layout <+> intAttrs
  where
    intAttrs = (Children =:: [])
               :& (BoxStyle =:: DefaultBox)
               :& RNil

-- | A record representing a widget of the _SelectionContainer class from IPython
defaultSelectionContainerWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr SelectionContainerClass
defaultSelectionContainerWidget viewName modelName layout = defaultBoxWidget viewName modelName layout <+> selAttrs
  where
    selAttrs = (Titles =:: [])
               :& (SelectedIndex =:: Nothing)
               :& (ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _Media class from IPython
defaultMediaWidget :: FieldType 'S.ViewName -> FieldType 'S.ModelName -> IPythonWidget 'LayoutType -> Rec Attr MediaClass
defaultMediaWidget viewName modelName layout = defaultCoreWidget <+> defaultDOMWidget viewName modelName layout <+> mediaAttrs
  where
    mediaAttrs = (BSValue =:: "")
                 :& RNil

-- | A record representing a widget of the Style class from IPython
defaultStyleWidget :: FieldType 'S.ModelName -> Rec Attr StyleWidgetClass
defaultStyleWidget modelName = (ModelName =:! modelName)
                              :& (ViewName =:! "StyleView")
                              :& (ViewModule =:! "@jupyter-widgets/base")
                              :& (ViewModuleVersion =:! "1.1.0")
                              :& (ModelModule =:! "@jupyter-widgets/controls")
                              :& (ModelModuleVersion =:! "1.4.0")
                              :& RNil

newtype WidgetState w = WidgetState { _getState :: Rec Attr (WidgetFields w) }

-- All records with ToPair instances for their Attrs will automatically have a toJSON instance now.
instance RecAll Attr (WidgetFields w) ToPairs => ToJSON (WidgetState w) where
  toJSON record =
    object
    . concat
      . recordToList
        . rmap (\(Compose (Dict x)) -> Const $ toPairs x) $ reifyConstraint (Proxy :: Proxy ToPairs) $ _getState
                                                                                                         record

data IPythonWidget (w :: WidgetType) =
       IPythonWidget
         { uuid :: UUID
         , state :: IORef (WidgetState w)
         }

-- | Change the value for a field, and notify the frontend about it. Doesn't work if the field is read only.
setField :: (f ∈ WidgetFields w, IHaskellWidget (IPythonWidget w), ToPairs (Attr f))
         => IPythonWidget w -> SField f -> FieldType f -> IO ()
setField widget sfield fval = do
  attr <- getAttr widget sfield
  when (_ro attr) $ error ("The field " ++ show sfield ++ " is read only")
  !newattr <- setField' widget sfield fval
  let pairs = toPairs newattr
  unless (null pairs) $ widgetSendUpdate widget (object pairs)

-- | Change the value of a field, without notifying the frontend and without checking if is read only. For internal use.
setField' :: (f ∈ WidgetFields w, IHaskellWidget (IPythonWidget w))
          => IPythonWidget w -> SField f -> FieldType f -> IO (Attr f)
setField' widget sfield val = do
  attr <- getAttr widget sfield
  newval <- _verify attr val
  let newattr = attr { _value = Real newval }
  modifyIORef (state widget) (WidgetState . rput newattr . _getState)
  return newattr

-- | Pluck an attribute from a record
getAttr :: (f ∈ WidgetFields w) => IPythonWidget w -> SField f -> IO (Attr f)
#if MIN_VERSION_vinyl(0,9,0)
getAttr widget _ = rget <$> _getState <$> readIORef (state widget)
#else
getAttr widget sfield = rget sfield <$> _getState <$> readIORef (state widget)
#endif

-- | Get the value of a field.
getField :: (f ∈ WidgetFields w) => IPythonWidget w -> SField f -> IO (FieldType f)
getField widget sfield = unwrap . _value <$> getAttr widget sfield

-- | Useful with toJSON and OverloadedStrings
str :: String -> String
str = id

properties :: IPythonWidget w -> IO ()
properties widget = do
  st <- readIORef $ state widget
  let convert :: Attr f -> Const (Field, TypeRep) f
      convert attr = Const (_field attr, getFieldType attr)

      renderRow (fname, ftype) = printf "%s ::: %s" (show fname) (show ftype)
      rows = map renderRow . recordToList . rmap convert $ _getState st
  mapM_ putStrLn rows

-- Helper function for widget to enforce their inability to fetch console input
noStdin :: IO a -> IO ()
noStdin action =
  let handler :: IOException -> IO ()
      handler e = when (ioeGetErrorType e == InvalidArgument)
                    (error "Widgets cannot do console input, sorry :)")
  in Ex.handle handler $ do
    nullFd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
    oldStdin <- dup stdInput
    void $ dupTo nullFd stdInput
    closeFd nullFd
    void action
    void $ dupTo oldStdin stdInput

-- Trigger events
triggerEvent :: (FieldType f ~ IO (), f ∈ WidgetFields w) => SField f -> IPythonWidget w -> IO ()
triggerEvent sfield w = noStdin . join $ getField w sfield

triggerChange :: ('S.ChangeHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerChange = triggerEvent ChangeHandler

triggerClick :: ('S.ClickHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerClick = triggerEvent ClickHandler

triggerSelection :: ('S.SelectionHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerSelection = triggerEvent SelectionHandler

triggerSubmit :: ('S.SubmitHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerSubmit = triggerEvent SubmitHandler

triggerDisplay :: ('S.DisplayHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerDisplay = triggerEvent DisplayHandler

-- | Every IHaskellWidget widget has the same IHaskellDisplay instance, for this
-- reason we need to use FlexibleContexts. The display implementation can still
-- be overriden per widget
instance IHaskellWidget (IPythonWidget w) => IHaskellDisplay (IPythonWidget w) where
  display b = do
    widgetSendView b -- Keeping compatibility with classic notebook
    return $ Display [ widgetdisplay $ unpack $ decodeUtf8 $ encode $ object [
      "model_id" .= getCommUUID b,
      "version_major" .= version_major,
      "version_minor" .= version_minor] ]
    where
      version_major = 2 :: Int
      version_minor = 0 :: Int

-- | The date class from IPython
data Date
  -- | No date specified. used by default
  = NullDate
  -- | Date year month day
  | Date Integer Integer Integer deriving (Eq,Ord)

defaultDate :: Date
defaultDate = NullDate

instance Show Date where
  show NullDate = "NullDate"
  show (Date y m d) = printf "%04d-%02d-%02d" y m d

instance ToJSON Date where
  toJSON NullDate = object []
  toJSON (Date y m d) = object [ "year" .= toJSON y
                               , "month" .= toJSON (m-1) -- In the frontend months go from 0 to 11
                               , "date" .= toJSON d
                               ]

instance FromJSON Date where
  parseJSON (Object v) = Date
    <$> v .: "year"
    <*> ((+1) <$> v .: "month")
    <*> v .: "date"
  parseJSON Null = pure NullDate
  parseJSON _ = mzero