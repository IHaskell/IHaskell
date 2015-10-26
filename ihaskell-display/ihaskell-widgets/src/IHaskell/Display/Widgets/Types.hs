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

module IHaskell.Display.Widgets.Types where

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
import           Control.Monad (unless, join, when, void)
import           Control.Applicative ((<$>))
import qualified Control.Exception as Ex
import           Data.Typeable (Typeable, TypeRep, typeOf)
import           Data.IORef (IORef, readIORef, modifyIORef)
import           Data.Text (Text, pack)
import           System.IO.Error
import           System.Posix.IO
import           Text.Printf (printf)

import           Data.Aeson
import           Data.Aeson.Types (Pair)

import           Data.Vinyl (Rec(..), (<+>), recordToList, reifyConstraint, rmap, Dict(..))
import           Data.Vinyl.Functor (Compose(..), Const(..))
import           Data.Vinyl.Lens (rget, rput, type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           Data.Singletons.Prelude ((:++))
import           Data.Singletons.TH

import           GHC.IO.Exception

import           IHaskell.Eval.Widgets (widgetSendUpdate)
import           IHaskell.Display (Base64, IHaskellWidget(..))
import           IHaskell.IPython.Message.UUID

import           IHaskell.Display.Widgets.Singletons (Field, SField)
import qualified IHaskell.Display.Widgets.Singletons as S
import           IHaskell.Display.Widgets.Common

-- Classes from IPython's widget hierarchy. Defined as such to reduce code duplication.
type WidgetClass = '[S.ViewModule, S.ViewName, S.ModelModule, S.ModelName,
  S.MsgThrottle, S.Version, S.DisplayHandler]

type DOMWidgetClass = WidgetClass :++ '[S.Visible, S.CSS, S.DOMClasses, S.Width, S.Height, S.Padding,
  S.Margin, S.Color, S.BackgroundColor, S.BorderColor, S.BorderWidth,
  S.BorderRadius, S.BorderStyle, S.FontStyle, S.FontWeight,
  S.FontSize, S.FontFamily]

type StringClass = DOMWidgetClass :++ '[S.StringValue, S.Disabled, S.Description, S.Placeholder]

type BoolClass = DOMWidgetClass :++ '[S.BoolValue, S.Disabled, S.Description, S.ChangeHandler]

type SelectionClass = DOMWidgetClass :++ '[S.Options, S.SelectedValue, S.SelectedLabel, S.Disabled,
  S.Description, S.SelectionHandler]

type MultipleSelectionClass = DOMWidgetClass :++ '[S.Options, S.SelectedValues, S.SelectedLabels, S.Disabled,
  S.Description, S.SelectionHandler]

type IntClass = DOMWidgetClass :++ '[S.IntValue, S.Disabled, S.Description, S.ChangeHandler]

type BoundedIntClass = IntClass :++ '[S.StepInt, S.MinInt, S.MaxInt]

type IntRangeClass = IntClass :++ '[S.IntPairValue, S.LowerInt, S.UpperInt]

type BoundedIntRangeClass = IntRangeClass :++ '[S.StepInt, S.MinInt, S.MaxInt]

type FloatClass = DOMWidgetClass :++ '[S.FloatValue, S.Disabled, S.Description, S.ChangeHandler]

type BoundedFloatClass = FloatClass :++ '[S.StepFloat, S.MinFloat, S.MaxFloat]

type FloatRangeClass = FloatClass :++ '[S.FloatPairValue, S.LowerFloat, S.UpperFloat]

type BoundedFloatRangeClass = FloatRangeClass :++ '[S.StepFloat, S.MinFloat, S.MaxFloat]

type BoxClass = DOMWidgetClass :++ '[S.Children, S.OverflowX, S.OverflowY, S.BoxStyle]

type SelectionContainerClass = BoxClass :++ '[S.Titles, S.SelectedIndex, S.ChangeHandler]

-- Types associated with Fields.

type family FieldType (f :: Field) :: * where
        FieldType S.ViewModule = Text
        FieldType S.ViewName = Text
        FieldType S.ModelModule = Text
        FieldType S.ModelName = Text
        FieldType S.MsgThrottle = Integer
        FieldType S.Version = Integer
        FieldType S.DisplayHandler = IO ()
        FieldType S.Visible = Bool
        FieldType S.CSS = [(Text, Text, Text)]
        FieldType S.DOMClasses = [Text]
        FieldType S.Width = PixCount
        FieldType S.Height = PixCount
        FieldType S.Padding = PixCount
        FieldType S.Margin = PixCount
        FieldType S.Color = Text
        FieldType S.BackgroundColor = Text
        FieldType S.BorderColor = Text
        FieldType S.BorderWidth = PixCount
        FieldType S.BorderRadius = PixCount
        FieldType S.BorderStyle = BorderStyleValue
        FieldType S.FontStyle = FontStyleValue
        FieldType S.FontWeight = FontWeightValue
        FieldType S.FontSize = PixCount
        FieldType S.FontFamily = Text
        FieldType S.Description = Text
        FieldType S.ClickHandler = IO ()
        FieldType S.SubmitHandler = IO ()
        FieldType S.Disabled = Bool
        FieldType S.StringValue = Text
        FieldType S.Placeholder = Text
        FieldType S.Tooltip = Text
        FieldType S.Icon = Text
        FieldType S.ButtonStyle = ButtonStyleValue
        FieldType S.B64Value = Base64
        FieldType S.ImageFormat = ImageFormatValue
        FieldType S.BoolValue = Bool
        FieldType S.Options = SelectionOptions
        FieldType S.SelectedLabel = Text
        FieldType S.SelectedValue = Text
        FieldType S.SelectionHandler = IO ()
        FieldType S.Tooltips = [Text]
        FieldType S.Icons = [Text]
        FieldType S.SelectedLabels = [Text]
        FieldType S.SelectedValues = [Text]
        FieldType S.IntValue = Integer
        FieldType S.StepInt = Integer
        FieldType S.MinInt = Integer
        FieldType S.MaxInt = Integer
        FieldType S.LowerInt = Integer
        FieldType S.UpperInt = Integer
        FieldType S.IntPairValue = (Integer, Integer)
        FieldType S.Orientation = OrientationValue
        FieldType S.ShowRange = Bool
        FieldType S.ReadOut = Bool
        FieldType S.SliderColor = Text
        FieldType S.BarStyle = BarStyleValue
        FieldType S.FloatValue = Double
        FieldType S.StepFloat = Double
        FieldType S.MinFloat = Double
        FieldType S.MaxFloat = Double
        FieldType S.LowerFloat = Double
        FieldType S.UpperFloat = Double
        FieldType S.FloatPairValue = (Double, Double)
        FieldType S.ChangeHandler = IO ()
        FieldType S.Children = [ChildWidget]
        FieldType S.OverflowX = OverflowValue
        FieldType S.OverflowY = OverflowValue
        FieldType S.BoxStyle = BoxStyleValue
        FieldType S.Flex = Int
        FieldType S.Pack = LocationValue
        FieldType S.Align = LocationValue
        FieldType S.Titles = [Text]
        FieldType S.SelectedIndex = Integer
        FieldType S.ReadOutMsg = Text
        FieldType S.Child = Maybe ChildWidget
        FieldType S.Selector = Text

-- | Can be used to put different widgets in a list. Useful for dealing with children widgets.
data ChildWidget = forall w. RecAll Attr (WidgetFields w) ToPairs => ChildWidget (IPythonWidget w)

instance ToJSON ChildWidget where
  toJSON (ChildWidget x) = toJSON . pack $ "IPY_MODEL_" ++ uuidToString (uuid x)

-- Will use a custom class rather than a newtype wrapper with an orphan instance. The main issue is
-- the need of a Bounded instance for Float / Double.
class CustomBounded a where
  lowerBound :: a
  upperBound :: a

-- Set according to what IPython widgets use
instance CustomBounded PixCount where
  upperBound = 10 ^ 16 - 1
  lowerBound = -(10 ^ 16 - 1)

instance CustomBounded Integer where
  lowerBound = -(10 ^ 16 - 1)
  upperBound = 10 ^ 16 - 1

instance CustomBounded Double where
  lowerBound = -(10 ** 16 - 1)
  upperBound = 10 ** 16 - 1

-- Different types of widgets. Every widget in IPython has a corresponding WidgetType
data WidgetType = ButtonType
                | ImageType
                | OutputType
                | HTMLType
                | LatexType
                | TextType
                | TextAreaType
                | CheckBoxType
                | ToggleButtonType
                | ValidType
                | DropdownType
                | RadioButtonsType
                | SelectType
                | ToggleButtonsType
                | SelectMultipleType
                | IntTextType
                | BoundedIntTextType
                | IntSliderType
                | IntProgressType
                | IntRangeSliderType
                | FloatTextType
                | BoundedFloatTextType
                | FloatSliderType
                | FloatProgressType
                | FloatRangeSliderType
                | BoxType
                | ProxyType
                | PlaceProxyType
                | FlexBoxType
                | AccordionType
                | TabType

-- Fields associated with a widget

type family WidgetFields (w :: WidgetType) :: [Field] where
        WidgetFields ButtonType =
                                DOMWidgetClass :++
                                  '[S.Description, S.Tooltip, S.Disabled, S.Icon, S.ButtonStyle,
                                    S.ClickHandler]
        WidgetFields ImageType =
                               DOMWidgetClass :++ '[S.ImageFormat, S.Width, S.Height, S.B64Value]
        WidgetFields OutputType = DOMWidgetClass
        WidgetFields HTMLType = StringClass
        WidgetFields LatexType = StringClass
        WidgetFields TextType =
                              StringClass :++ '[S.SubmitHandler, S.ChangeHandler]
        WidgetFields TextAreaType = StringClass :++ '[S.ChangeHandler]
        WidgetFields CheckBoxType = BoolClass
        WidgetFields ToggleButtonType =
                                      BoolClass :++ '[S.Tooltip, S.Icon, S.ButtonStyle]
        WidgetFields ValidType = BoolClass :++ '[S.ReadOutMsg]
        WidgetFields DropdownType = SelectionClass :++ '[S.ButtonStyle]
        WidgetFields RadioButtonsType = SelectionClass
        WidgetFields SelectType = SelectionClass
        WidgetFields ToggleButtonsType =
                                       SelectionClass :++ '[S.Tooltips, S.Icons, S.ButtonStyle]
        WidgetFields SelectMultipleType = MultipleSelectionClass
        WidgetFields IntTextType = IntClass
        WidgetFields BoundedIntTextType = BoundedIntClass
        WidgetFields IntSliderType =
                                   BoundedIntClass :++
                                     '[S.Orientation, S.ShowRange, S.ReadOut, S.SliderColor]
        WidgetFields IntProgressType =
                                     BoundedIntClass :++ '[S.Orientation, S.BarStyle]
        WidgetFields IntRangeSliderType =
                                        BoundedIntRangeClass :++
                                          '[S.Orientation, S.ShowRange, S.ReadOut, S.SliderColor]
        WidgetFields FloatTextType = FloatClass
        WidgetFields BoundedFloatTextType = BoundedFloatClass
        WidgetFields FloatSliderType =
                                     BoundedFloatClass :++
                                       '[S.Orientation, S.ShowRange, S.ReadOut, S.SliderColor]
        WidgetFields FloatProgressType =
                                       BoundedFloatClass :++ '[S.Orientation, S.BarStyle]
        WidgetFields FloatRangeSliderType =
                                          BoundedFloatRangeClass :++
                                            '[S.Orientation, S.ShowRange, S.ReadOut, S.SliderColor]
        WidgetFields BoxType = BoxClass
        WidgetFields ProxyType = WidgetClass :++ '[S.Child]
        WidgetFields PlaceProxyType =
                                    WidgetFields ProxyType :++ '[S.Selector]
        WidgetFields FlexBoxType =
                                 BoxClass :++ '[S.Orientation, S.Flex, S.Pack, S.Align]
        WidgetFields AccordionType = SelectionContainerClass
        WidgetFields TabType = SelectionContainerClass

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
          } -> Attr f

getFieldType :: Attr f -> TypeRep
getFieldType Attr { _value = attrval } = typeOf $ unwrap attrval

instance ToJSON (FieldType f) => ToJSON (Attr f) where
  toJSON attr =
    case _value attr of
      Dummy _ -> ""
      Real x  -> toJSON x

-- Types that can be converted to Aeson Pairs.
class ToPairs a where
  toPairs :: a -> [Pair]

-- Attributes that aren't synced with the frontend give [] on toPairs
instance ToPairs (Attr S.ViewModule) where
  toPairs x = ["_view_module" .= toJSON x]

instance ToPairs (Attr S.ViewName) where
  toPairs x = ["_view_name" .= toJSON x]

instance ToPairs (Attr S.ModelModule) where
  toPairs x = ["_model_module" .= toJSON x]

instance ToPairs (Attr S.ModelName) where
  toPairs x = ["_model_name" .= toJSON x]

instance ToPairs (Attr S.MsgThrottle) where
  toPairs x = ["msg_throttle" .= toJSON x]

instance ToPairs (Attr S.Version) where
  toPairs x = ["version" .= toJSON x]

instance ToPairs (Attr S.DisplayHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr S.Visible) where
  toPairs x = ["visible" .= toJSON x]

instance ToPairs (Attr S.CSS) where
  toPairs x = ["_css" .= toJSON x]

instance ToPairs (Attr S.DOMClasses) where
  toPairs x = ["_dom_classes" .= toJSON x]

instance ToPairs (Attr S.Width) where
  toPairs x = ["width" .= toJSON x]

instance ToPairs (Attr S.Height) where
  toPairs x = ["height" .= toJSON x]

instance ToPairs (Attr S.Padding) where
  toPairs x = ["padding" .= toJSON x]

instance ToPairs (Attr S.Margin) where
  toPairs x = ["margin" .= toJSON x]

instance ToPairs (Attr S.Color) where
  toPairs x = ["color" .= toJSON x]

instance ToPairs (Attr S.BackgroundColor) where
  toPairs x = ["background_color" .= toJSON x]

instance ToPairs (Attr S.BorderColor) where
  toPairs x = ["border_color" .= toJSON x]

instance ToPairs (Attr S.BorderWidth) where
  toPairs x = ["border_width" .= toJSON x]

instance ToPairs (Attr S.BorderRadius) where
  toPairs x = ["border_radius" .= toJSON x]

instance ToPairs (Attr S.BorderStyle) where
  toPairs x = ["border_style" .= toJSON x]

instance ToPairs (Attr S.FontStyle) where
  toPairs x = ["font_style" .= toJSON x]

instance ToPairs (Attr S.FontWeight) where
  toPairs x = ["font_weight" .= toJSON x]

instance ToPairs (Attr S.FontSize) where
  toPairs x = ["font_size" .= toJSON x]

instance ToPairs (Attr S.FontFamily) where
  toPairs x = ["font_family" .= toJSON x]

instance ToPairs (Attr S.Description) where
  toPairs x = ["description" .= toJSON x]

instance ToPairs (Attr S.ClickHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr S.SubmitHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr S.Disabled) where
  toPairs x = ["disabled" .= toJSON x]

instance ToPairs (Attr S.StringValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.Placeholder) where
  toPairs x = ["placeholder" .= toJSON x]

instance ToPairs (Attr S.Tooltip) where
  toPairs x = ["tooltip" .= toJSON x]

instance ToPairs (Attr S.Icon) where
  toPairs x = ["icon" .= toJSON x]

instance ToPairs (Attr S.ButtonStyle) where
  toPairs x = ["button_style" .= toJSON x]

instance ToPairs (Attr S.B64Value) where
  toPairs x = ["_b64value" .= toJSON x]

instance ToPairs (Attr S.ImageFormat) where
  toPairs x = ["format" .= toJSON x]

instance ToPairs (Attr S.BoolValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.SelectedLabel) where
  toPairs x = ["selected_label" .= toJSON x]

instance ToPairs (Attr S.SelectedValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.Options) where
  toPairs x =
    case _value x of
      Dummy _                -> labels ("" :: Text)
      Real (OptionLabels xs) -> labels xs
      Real (OptionDict xps)  -> labels $ map fst xps
    where
      labels xs = ["_options_labels" .= xs]

instance ToPairs (Attr S.SelectionHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr S.Tooltips) where
  toPairs x = ["tooltips" .= toJSON x]

instance ToPairs (Attr S.Icons) where
  toPairs x = ["icons" .= toJSON x]

instance ToPairs (Attr S.SelectedLabels) where
  toPairs x = ["selected_labels" .= toJSON x]

instance ToPairs (Attr S.SelectedValues) where
  toPairs x = ["values" .= toJSON x]

instance ToPairs (Attr S.IntValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.StepInt) where
  toPairs x = ["step" .= toJSON x]

instance ToPairs (Attr S.MinInt) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr S.MaxInt) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr S.IntPairValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.LowerInt) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr S.UpperInt) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr S.FloatValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.StepFloat) where
  toPairs x = ["step" .= toJSON x]

instance ToPairs (Attr S.MinFloat) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr S.MaxFloat) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr S.FloatPairValue) where
  toPairs x = ["value" .= toJSON x]

instance ToPairs (Attr S.LowerFloat) where
  toPairs x = ["min" .= toJSON x]

instance ToPairs (Attr S.UpperFloat) where
  toPairs x = ["max" .= toJSON x]

instance ToPairs (Attr S.Orientation) where
  toPairs x = ["orientation" .= toJSON x]

instance ToPairs (Attr S.ShowRange) where
  toPairs x = ["_range" .= toJSON x]

instance ToPairs (Attr S.ReadOut) where
  toPairs x = ["readout" .= toJSON x]

instance ToPairs (Attr S.SliderColor) where
  toPairs x = ["slider_color" .= toJSON x]

instance ToPairs (Attr S.BarStyle) where
  toPairs x = ["bar_style" .= toJSON x]

instance ToPairs (Attr S.ChangeHandler) where
  toPairs _ = [] -- Not sent to the frontend

instance ToPairs (Attr S.Children) where
  toPairs x = ["children" .= toJSON x]

instance ToPairs (Attr S.OverflowX) where
  toPairs x = ["overflow_x" .= toJSON x]

instance ToPairs (Attr S.OverflowY) where
  toPairs x = ["overflow_y" .= toJSON x]

instance ToPairs (Attr S.BoxStyle) where
  toPairs x = ["box_style" .= toJSON x]

instance ToPairs (Attr S.Flex) where
  toPairs x = ["flex" .= toJSON x]

instance ToPairs (Attr S.Pack) where
  toPairs x = ["pack" .= toJSON x]

instance ToPairs (Attr S.Align) where
  toPairs x = ["align" .= toJSON x]

instance ToPairs (Attr S.Titles) where
  toPairs x = ["_titles" .= toJSON x]

instance ToPairs (Attr S.SelectedIndex) where
  toPairs x = ["selected_index" .= toJSON x]

instance ToPairs (Attr S.ReadOutMsg) where
  toPairs x = ["readout" .= toJSON x]

instance ToPairs (Attr S.Child) where
  toPairs x = ["child" .= toJSON x]

instance ToPairs (Attr S.Selector) where
  toPairs x = ["selector" .= toJSON x]

-- | Store the value for a field, as an object parametrized by the Field. No verification is done
-- for these values.
(=::) :: (SingI f, Typeable (FieldType f)) => Sing f -> FieldType f -> Attr f
s =:: x = Attr { _value = Real x, _verify = return, _field = reflect s }

-- | If the number is in the range, return it. Otherwise raise the appropriate (over/under)flow
-- exception.
rangeCheck :: (Num a, Ord a) => (a, a) -> a -> IO a
rangeCheck (l, u) x
  | l <= x && x <= u = return x
  | l > x = Ex.throw Ex.Underflow
  | u < x = Ex.throw Ex.Overflow
  | otherwise = error "The impossible happened in IHaskell.Display.Widgets.Types.rangeCheck"

-- | Store a numeric value, with verification mechanism for its range.
ranged :: (SingI f, Num (FieldType f), Ord (FieldType f), Typeable (FieldType f))
       => Sing f -> (FieldType f, FieldType f) -> AttrVal (FieldType f) -> Attr f
ranged s range x = Attr x (rangeCheck range) (reflect s)

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

-- | Get a field from a singleton Adapted from: http://stackoverflow.com/a/28033250/2388535
reflect :: forall (f :: Field). (SingI f, SingKind ('KProxy :: KProxy Field)) => Sing f -> Field
reflect = fromSing

-- | A record representing an object of the Widget class from IPython
defaultWidget :: FieldType S.ViewName -> Rec Attr WidgetClass
defaultWidget viewName = (ViewModule =:: "")
                         :& (ViewName =:: viewName)
                         :& (ModelModule =:: "")
                         :& (ModelName =:: "WidgetModel")
                         :& (MsgThrottle =:+ 3)
                         :& (Version =:: 0)
                         :& (DisplayHandler =:: return ())
                         :& RNil

-- | A record representing an object of the DOMWidget class from IPython
defaultDOMWidget :: FieldType S.ViewName -> Rec Attr DOMWidgetClass
defaultDOMWidget viewName = defaultWidget viewName <+> domAttrs
  where
    domAttrs = (Visible =:: True)
               :& (CSS =:: [])
               :& (DOMClasses =:: [])
               :& (Width =:+ 0)
               :& (Height =:+ 0)
               :& (Padding =:+ 0)
               :& (Margin =:+ 0)
               :& (Color =:: "")
               :& (BackgroundColor =:: "")
               :& (BorderColor =:: "")
               :& (BorderWidth =:+ 0)
               :& (BorderRadius =:+ 0)
               :& (BorderStyle =:: DefaultBorder)
               :& (FontStyle =:: DefaultFont)
               :& (FontWeight =:: DefaultWeight)
               :& (FontSize =:+ 0)
               :& (FontFamily =:: "")
               :& RNil

-- | A record representing a widget of the _String class from IPython
defaultStringWidget :: FieldType S.ViewName -> Rec Attr StringClass
defaultStringWidget viewName = defaultDOMWidget viewName <+> strAttrs
  where
    strAttrs = (StringValue =:: "")
               :& (Disabled =:: False)
               :& (Description =:: "")
               :& (Placeholder =:: "")
               :& RNil

-- | A record representing a widget of the _Bool class from IPython
defaultBoolWidget :: FieldType S.ViewName -> Rec Attr BoolClass
defaultBoolWidget viewName = defaultDOMWidget viewName <+> boolAttrs
  where
    boolAttrs = (BoolValue =:: False)
                :& (Disabled =:: False)
                :& (Description =:: "")
                :& (ChangeHandler =:: return ())
                :& RNil

-- | A record representing a widget of the _Selection class from IPython
defaultSelectionWidget :: FieldType S.ViewName -> Rec Attr SelectionClass
defaultSelectionWidget viewName = defaultDOMWidget viewName <+> selectionAttrs
  where
    selectionAttrs = (Options =:: OptionLabels [])
                     :& (SelectedValue =:: "")
                     :& (SelectedLabel =:: "")
                     :& (Disabled =:: False)
                     :& (Description =:: "")
                     :& (SelectionHandler =:: return ())
                     :& RNil

-- | A record representing a widget of the _MultipleSelection class from IPython
defaultMultipleSelectionWidget :: FieldType S.ViewName -> Rec Attr MultipleSelectionClass
defaultMultipleSelectionWidget viewName = defaultDOMWidget viewName <+> mulSelAttrs
  where
    mulSelAttrs = (Options =:: OptionLabels [])
                  :& (SelectedValues =:: [])
                  :& (SelectedLabels =:: [])
                  :& (Disabled =:: False)
                  :& (Description =:: "")
                  :& (SelectionHandler =:: return ())
                  :& RNil

-- | A record representing a widget of the _Int class from IPython
defaultIntWidget :: FieldType S.ViewName -> Rec Attr IntClass
defaultIntWidget viewName = defaultDOMWidget viewName <+> intAttrs
  where
    intAttrs = (IntValue =:: 0)
               :& (Disabled =:: False)
               :& (Description =:: "")
               :& (ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultBoundedIntWidget :: FieldType S.ViewName -> Rec Attr BoundedIntClass
defaultBoundedIntWidget viewName = defaultIntWidget viewName <+> boundedIntAttrs
  where
    boundedIntAttrs = (StepInt =:: 1)
                      :& (MinInt =:: 0)
                      :& (MaxInt =:: 100)
                      :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultIntRangeWidget :: FieldType S.ViewName -> Rec Attr IntRangeClass
defaultIntRangeWidget viewName = defaultIntWidget viewName <+> rangeAttrs
  where
    rangeAttrs = (IntPairValue =:: (25, 75))
                 :& (LowerInt =:: 0)
                 :& (UpperInt =:: 100)
                 :& RNil

-- | A record representing a widget of the _BoundedIntRange class from IPython
defaultBoundedIntRangeWidget :: FieldType S.ViewName -> Rec Attr BoundedIntRangeClass
defaultBoundedIntRangeWidget viewName = defaultIntRangeWidget viewName <+> boundedIntRangeAttrs
  where
    boundedIntRangeAttrs = (StepInt =:+ 1)
                           :& (MinInt =:: 0)
                           :& (MaxInt =:: 100)
                           :& RNil

-- | A record representing a widget of the _Float class from IPython
defaultFloatWidget :: FieldType S.ViewName -> Rec Attr FloatClass
defaultFloatWidget viewName = defaultDOMWidget viewName <+> intAttrs
  where
    intAttrs = (FloatValue =:: 0)
               :& (Disabled =:: False)
               :& (Description =:: "")
               :& (ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultBoundedFloatWidget :: FieldType S.ViewName -> Rec Attr BoundedFloatClass
defaultBoundedFloatWidget viewName = defaultFloatWidget viewName <+> boundedFloatAttrs
  where
    boundedFloatAttrs = (StepFloat =:+ 1)
                        :& (MinFloat =:: 0)
                        :& (MaxFloat =:: 100)
                        :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultFloatRangeWidget :: FieldType S.ViewName -> Rec Attr FloatRangeClass
defaultFloatRangeWidget viewName = defaultFloatWidget viewName <+> rangeAttrs
  where
    rangeAttrs = (FloatPairValue =:: (25, 75))
                 :& (LowerFloat =:: 0)
                 :& (UpperFloat =:: 100)
                 :& RNil

-- | A record representing a widget of the _BoundedFloatRange class from IPython
defaultBoundedFloatRangeWidget :: FieldType S.ViewName -> Rec Attr BoundedFloatRangeClass
defaultBoundedFloatRangeWidget viewName = defaultFloatRangeWidget viewName <+> boundedFloatRangeAttrs
  where
    boundedFloatRangeAttrs = (StepFloat =:+ 1)
                             :& (MinFloat =:: 0)
                             :& (MaxFloat =:: 100)
                             :& RNil

-- | A record representing a widget of the _Box class from IPython
defaultBoxWidget :: FieldType S.ViewName -> Rec Attr BoxClass
defaultBoxWidget viewName = domAttrs <+> boxAttrs
  where
    defaultDOM = defaultDOMWidget viewName
    domAttrs = rput (ModelName =:: "BoxModel") defaultDOM
    boxAttrs = (Children =:: [])
               :& (OverflowX =:: DefaultOverflow)
               :& (OverflowY =:: DefaultOverflow)
               :& (BoxStyle =:: DefaultBox)
               :& RNil

-- | A record representing a widget of the _SelectionContainer class from IPython
defaultSelectionContainerWidget :: FieldType S.ViewName -> Rec Attr SelectionContainerClass
defaultSelectionContainerWidget viewName = defaultBoxWidget viewName <+> selAttrs
  where
    selAttrs = (Titles =:: [])
               :& (SelectedIndex =:: 0)
               :& (ChangeHandler =:: return ())
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

-- | Change the value for a field, and notify the frontend about it.
setField :: (f ∈ WidgetFields w, IHaskellWidget (IPythonWidget w), ToPairs (Attr f))
         => IPythonWidget w -> SField f -> FieldType f -> IO ()
setField widget sfield fval = do
  !newattr <- setField' widget sfield fval
  let pairs = toPairs newattr
  unless (null pairs) $ widgetSendUpdate widget (object pairs)

-- | Change the value of a field, without notifying the frontend. For internal use.
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
getAttr widget sfield = rget sfield <$> _getState <$> readIORef (state widget)

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

triggerChange :: (S.ChangeHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerChange = triggerEvent ChangeHandler

triggerClick :: (S.ClickHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerClick = triggerEvent ClickHandler

triggerSelection :: (S.SelectionHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerSelection = triggerEvent SelectionHandler

triggerSubmit :: (S.SubmitHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerSubmit = triggerEvent SubmitHandler

triggerDisplay :: (S.DisplayHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerDisplay = triggerEvent DisplayHandler
