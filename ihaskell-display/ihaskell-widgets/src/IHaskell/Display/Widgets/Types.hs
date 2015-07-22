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
module IHaskell.Display.Widgets.Types where

-- | This module houses all the type-trickery needed to make widgets happen.
--
-- All widgets have a corresponding 'WidgetType', and some fields/attributes/properties as defined by
-- the 'WidgetFields' type-family.
--
-- Each widget field corresponds to a concrete haskell type, as given by the 'FieldType' type-family.
--
-- Vinyl records are used to wrap together widget fields into a single 'WidgetState'.
--
-- Singletons are used as a way to represent the promoted types of kind Field. For example:
--
-- @
-- SViewName :: SField ViewName
-- @
--
-- This allows the user to pass the type 'ViewName' without using Data.Proxy. In essence, a singleton
-- is the only inhabitant (other than bottom) of a promoted type. Single element set/type == singleton.
--
-- It also allows the record to wrap values of properties with information about their Field type. A
-- vinyl record is represented as @Rec f ts@, which means that a record is a list of @f x@, where @x@
-- is a type present in the type-level list @ts@. Thus a 'WidgetState' is essentially a list of field
-- properties wrapped together with the corresponding promoted Field type. See ('=::') for more.
--
-- The properties function can be used to view all the @Field@s associated with a widget object.
--
-- Attributes are represented by the @Attr@ data type, which holds the value of a field, along with
-- the actual @Field@ object and a function to verify validity of changes to the value.
--
-- The IPython widgets expect state updates of the form {"property": value}, where an empty string for
-- numeric values is ignored by the frontend and the default value is used instead. Some numbers need to
-- be sent as numbers (represented by @Integer@), whereas some need to be sent as Strings (@StrInt@).
--
-- Child widgets are expected to be sent as strings of the form "IPY_MODEL_<uuid>", where @<uuid>@
-- represents the uuid of the widget's comm.
--
-- To know more about the IPython messaging specification (as implemented in this package) take a look
-- at the supplied MsgSpec.md.

import Control.Monad (unless, join, when, void)
import Control.Applicative ((<$>))
import qualified Control.Exception as Ex

import GHC.IO.Exception
import System.IO.Error
import System.Posix.IO

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Text (Text, pack)

import Data.Vinyl (Rec (..), (<+>), recordToList, reifyConstraint, rmap, Dict (..))
import Data.Vinyl.Functor (Compose (..), Const (..))
import Data.Vinyl.Lens (rget, rput, type (∈))
import Data.Vinyl.TypeLevel (RecAll)

import Data.Singletons.Prelude ((:++))
import Data.Singletons.TH

import IHaskell.Eval.Widgets (widgetSendUpdate)
import IHaskell.Display (Base64, IHaskellWidget (..))
import IHaskell.IPython.Message.UUID

import IHaskell.Display.Widgets.Common

-- Classes from IPython's widget hierarchy. Defined as such to reduce code duplication.
type WidgetClass = '[ViewModule, ViewName, MsgThrottle, Version, DisplayHandler]
type DOMWidgetClass = WidgetClass :++
   '[ Visible, CSS, DOMClasses, Width, Height, Padding, Margin, Color
    , BackgroundColor, BorderColor, BorderWidth, BorderRadius, BorderStyle, FontStyle
    , FontWeight, FontSize, FontFamily
    ]
type StringClass = DOMWidgetClass :++ '[StringValue, Disabled, Description, Placeholder]
type BoolClass = DOMWidgetClass :++ '[BoolValue, Disabled, Description, ChangeHandler]
type SelectionClass = DOMWidgetClass :++
   '[Options, SelectedValue, SelectedLabel, Disabled, Description, SelectionHandler]
type MultipleSelectionClass = DOMWidgetClass :++
   '[Options, SelectedLabels, SelectedValues, Disabled, Description, SelectionHandler]
type IntClass = DOMWidgetClass :++ '[IntValue, Disabled, Description, ChangeHandler]
type BoundedIntClass = IntClass :++ '[StepInt, MinInt, MaxInt]
type IntRangeClass = IntClass :++ '[IntPairValue, LowerInt, UpperInt]
type BoundedIntRangeClass = IntRangeClass :++ '[StepInt, MinInt, MaxInt]
type FloatClass = DOMWidgetClass :++ '[FloatValue, Disabled, Description, ChangeHandler]
type BoundedFloatClass = FloatClass :++ '[StepFloat, MinFloat, MaxFloat]
type FloatRangeClass = FloatClass :++ '[FloatPairValue, LowerFloat, UpperFloat]
type BoundedFloatRangeClass = FloatRangeClass :++ '[StepFloat, MinFloat, MaxFloat]
type BoxClass = DOMWidgetClass :++ '[Children, OverflowX, OverflowY, BoxStyle]
type SelectionContainerClass = BoxClass :++ '[Titles, SelectedIndex, ChangeHandler]

-- Types associated with Fields.
type family FieldType (f :: Field) :: * where
  FieldType ViewModule = Text
  FieldType ViewName = Text
  FieldType MsgThrottle = Integer
  FieldType Version = Integer
  FieldType DisplayHandler = IO ()
  FieldType Visible = Bool
  FieldType CSS = [(Text, Text, Text)]
  FieldType DOMClasses = [Text]
  FieldType Width = StrInt
  FieldType Height = StrInt
  FieldType Padding = StrInt
  FieldType Margin = StrInt
  FieldType Color = Text
  FieldType BackgroundColor = Text
  FieldType BorderColor = Text
  FieldType BorderWidth = StrInt
  FieldType BorderRadius = StrInt
  FieldType BorderStyle = BorderStyleValue
  FieldType FontStyle = FontStyleValue
  FieldType FontWeight = FontWeightValue
  FieldType FontSize = StrInt
  FieldType FontFamily = Text
  FieldType Description = Text
  FieldType ClickHandler = IO ()
  FieldType SubmitHandler = IO ()
  FieldType Disabled = Bool
  FieldType StringValue = Text
  FieldType Placeholder = Text
  FieldType Tooltip = Text
  FieldType Icon = Text
  FieldType ButtonStyle = ButtonStyleValue
  FieldType B64Value = Base64
  FieldType ImageFormat = ImageFormatValue
  FieldType BoolValue = Bool
  FieldType Options = SelectionOptions
  FieldType SelectedLabel = Text
  FieldType SelectedValue = Text
  FieldType SelectionHandler = IO ()
  FieldType Tooltips = [Text]
  FieldType Icons = [Text]
  FieldType SelectedLabels = [Text]
  FieldType SelectedValues = [Text]
  FieldType IntValue = Integer
  FieldType StepInt = Integer
  FieldType MinInt = Integer
  FieldType MaxInt = Integer
  FieldType LowerInt = Integer
  FieldType UpperInt = Integer
  FieldType IntPairValue = (Integer, Integer)
  FieldType Orientation = OrientationValue
  FieldType ShowRange = Bool
  FieldType ReadOut = Bool
  FieldType SliderColor = Text
  FieldType BarStyle = BarStyleValue
  FieldType FloatValue = Double
  FieldType StepFloat = Double
  FieldType MinFloat = Double
  FieldType MaxFloat = Double
  FieldType LowerFloat = Double
  FieldType UpperFloat = Double
  FieldType FloatPairValue = (Double, Double)
  FieldType ChangeHandler = IO ()
  FieldType Children = [ChildWidget]
  FieldType OverflowX = OverflowValue
  FieldType OverflowY = OverflowValue
  FieldType BoxStyle = BoxStyleValue
  FieldType Flex = Int
  FieldType Pack = LocationValue
  FieldType Align = LocationValue
  FieldType Titles = [Text]
  FieldType SelectedIndex = Integer

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
instance CustomBounded StrInt where
  upperBound = 10 ^ 16 - 1
  lowerBound = - (10 ^ 16 - 1)

instance CustomBounded Integer where
  lowerBound = - (10 ^ 16 - 1)
  upperBound = 10 ^ 16 - 1

instance CustomBounded Double where
  lowerBound = - (10 ** 16 - 1)
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
                | FlexBoxType
                | AccordionType
                | TabType

-- Fields associated with a widget
type family WidgetFields (w :: WidgetType) :: [Field] where
  WidgetFields ButtonType = DOMWidgetClass :++ '[Description, Tooltip, Disabled, Icon, ButtonStyle, ClickHandler]
  WidgetFields ImageType = DOMWidgetClass :++ '[ImageFormat, B64Value]
  WidgetFields OutputType = DOMWidgetClass
  WidgetFields HTMLType = StringClass
  WidgetFields LatexType = StringClass
  WidgetFields TextType = StringClass :++ '[SubmitHandler, ChangeHandler]
  WidgetFields TextAreaType = StringClass :++ '[ChangeHandler]
  WidgetFields CheckBoxType = BoolClass
  WidgetFields ToggleButtonType = BoolClass :++ '[Tooltip, Icon, ButtonStyle]
  WidgetFields DropdownType = SelectionClass :++ '[ButtonStyle]
  WidgetFields RadioButtonsType = SelectionClass
  WidgetFields SelectType = SelectionClass
  WidgetFields ToggleButtonsType = SelectionClass :++ '[Tooltips, Icons, ButtonStyle]
  WidgetFields SelectMultipleType = MultipleSelectionClass
  WidgetFields IntTextType = IntClass
  WidgetFields BoundedIntTextType = BoundedIntClass
  WidgetFields IntSliderType = BoundedIntClass :++ '[Orientation, ShowRange, ReadOut, SliderColor]
  WidgetFields IntProgressType = BoundedIntClass :++ '[BarStyle]
  WidgetFields IntRangeSliderType = BoundedIntRangeClass :++ '[Orientation, ShowRange, ReadOut, SliderColor]
  WidgetFields FloatTextType = FloatClass
  WidgetFields BoundedFloatTextType = BoundedFloatClass
  WidgetFields FloatSliderType = BoundedFloatClass :++ '[Orientation, ShowRange, ReadOut, SliderColor]
  WidgetFields FloatProgressType = BoundedFloatClass :++ '[BarStyle]
  WidgetFields FloatRangeSliderType = BoundedFloatRangeClass :++ '[Orientation, ShowRange, ReadOut, SliderColor]
  WidgetFields BoxType = BoxClass
  WidgetFields FlexBoxType = BoxClass :++ '[Orientation, Flex, Pack, Align]
  WidgetFields AccordionType = SelectionContainerClass
  WidgetFields TabType = SelectionContainerClass

-- Wrapper around a field's value. A dummy value is sent as an empty string to the frontend.
data AttrVal a = Dummy a | Real a

unwrap :: AttrVal a -> a
unwrap (Dummy x) = x
unwrap (Real x) = x

-- Wrapper around a field.
data Attr (f :: Field) =
  Attr { _value :: AttrVal (FieldType f)
       , _verify :: FieldType f -> IO (FieldType f)
       , _field :: Field
       }

instance ToJSON (FieldType f) => ToJSON (Attr f) where
  toJSON attr = case _value attr of
    Dummy _ -> ""
    Real x -> toJSON x

-- Types that can be converted to Aeson Pairs.
class ToPairs a where
  toPairs :: a -> [Pair]

-- Attributes that aren't synced with the frontend give [] on toPairs
instance ToPairs (Attr ViewModule) where toPairs x = ["_view_module" .= toJSON x]
instance ToPairs (Attr ViewName) where toPairs x = ["_view_name" .= toJSON x]
instance ToPairs (Attr MsgThrottle) where toPairs x = ["msg_throttle" .= toJSON x]
instance ToPairs (Attr Version) where toPairs x = ["version" .= toJSON x]
instance ToPairs (Attr DisplayHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Visible) where toPairs x = ["visible" .= toJSON x]
instance ToPairs (Attr CSS) where toPairs x = ["_css" .= toJSON x]
instance ToPairs (Attr DOMClasses) where toPairs x = ["_dom_classes" .= toJSON x]
instance ToPairs (Attr Width) where toPairs x = ["width" .= toJSON x]
instance ToPairs (Attr Height) where toPairs x = ["height" .= toJSON x]
instance ToPairs (Attr Padding) where toPairs x = ["padding" .= toJSON x]
instance ToPairs (Attr Margin) where toPairs x = ["margin" .= toJSON x]
instance ToPairs (Attr Color) where toPairs x = ["color" .= toJSON x]
instance ToPairs (Attr BackgroundColor) where toPairs x = ["background_color" .= toJSON x]
instance ToPairs (Attr BorderColor) where toPairs x = ["border_color" .= toJSON x]
instance ToPairs (Attr BorderWidth) where toPairs x = ["border_width" .= toJSON x]
instance ToPairs (Attr BorderRadius) where toPairs x = ["border_radius" .= toJSON x]
instance ToPairs (Attr BorderStyle) where toPairs x = ["border_style" .= toJSON x]
instance ToPairs (Attr FontStyle) where toPairs x = ["font_style" .= toJSON x]
instance ToPairs (Attr FontWeight) where toPairs x = ["font_weight" .= toJSON x]
instance ToPairs (Attr FontSize) where toPairs x = ["font_size" .= toJSON x]
instance ToPairs (Attr FontFamily) where toPairs x = ["font_family" .= toJSON x]
instance ToPairs (Attr Description) where toPairs x = ["description" .= toJSON x]
instance ToPairs (Attr ClickHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr SubmitHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Disabled) where toPairs x = ["disabled" .= toJSON x]
instance ToPairs (Attr StringValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr Placeholder) where toPairs x = ["placeholder" .= toJSON x]
instance ToPairs (Attr Tooltip) where toPairs x = ["tooltip" .= toJSON x]
instance ToPairs (Attr Icon) where toPairs x = ["icon" .= toJSON x]
instance ToPairs (Attr ButtonStyle) where toPairs x = ["button_style" .= toJSON x]
instance ToPairs (Attr B64Value) where toPairs x = ["_b64value" .= toJSON x]
instance ToPairs (Attr ImageFormat) where toPairs x = ["format" .= toJSON x]
instance ToPairs (Attr BoolValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr SelectedLabel) where toPairs x = ["selected_label" .= toJSON x]
instance ToPairs (Attr SelectedValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr Options) where
  toPairs x = case _value x of
    Dummy _ -> labels ("" :: Text)
    Real (OptionLabels xs) -> labels xs
    Real (OptionDict xps) -> labels $ map fst xps
    where labels xs = ["_options_labels" .= xs]
instance ToPairs (Attr SelectionHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Tooltips) where toPairs x = ["tooltips" .= toJSON x]
instance ToPairs (Attr Icons) where toPairs x = ["icons" .= toJSON x]
instance ToPairs (Attr SelectedLabels) where toPairs x = ["selected_labels" .= toJSON x]
instance ToPairs (Attr SelectedValues) where toPairs x = ["values" .= toJSON x]
instance ToPairs (Attr IntValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr StepInt) where toPairs x = ["step" .= toJSON x]
instance ToPairs (Attr MinInt) where toPairs x = ["min" .= toJSON x]
instance ToPairs (Attr MaxInt) where toPairs x = ["max" .= toJSON x]
instance ToPairs (Attr IntPairValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr LowerInt) where toPairs x = ["min" .= toJSON x]
instance ToPairs (Attr UpperInt) where toPairs x = ["max" .= toJSON x]
instance ToPairs (Attr FloatValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr StepFloat) where toPairs x = ["step" .= toJSON x]
instance ToPairs (Attr MinFloat) where toPairs x = ["min" .= toJSON x]
instance ToPairs (Attr MaxFloat) where toPairs x = ["max" .= toJSON x]
instance ToPairs (Attr FloatPairValue) where toPairs x = ["value" .= toJSON x]
instance ToPairs (Attr LowerFloat) where toPairs x = ["min" .= toJSON x]
instance ToPairs (Attr UpperFloat) where toPairs x = ["max" .= toJSON x]
instance ToPairs (Attr Orientation) where toPairs x = ["orientation" .= toJSON x]
instance ToPairs (Attr ShowRange) where toPairs x = ["_range" .= toJSON x]
instance ToPairs (Attr ReadOut) where toPairs x = ["readout" .= toJSON x]
instance ToPairs (Attr SliderColor) where toPairs x = ["slider_color" .= toJSON x]
instance ToPairs (Attr BarStyle) where toPairs x = ["bar_style" .= toJSON x]
instance ToPairs (Attr ChangeHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Children) where toPairs x = ["children" .= toJSON x]
instance ToPairs (Attr OverflowX) where toPairs x = ["overflow_x" .= toJSON x]
instance ToPairs (Attr OverflowY) where toPairs x = ["overflow_y" .= toJSON x]
instance ToPairs (Attr BoxStyle) where toPairs x = ["box_style" .= toJSON x]
instance ToPairs (Attr Flex) where toPairs x = ["flex" .= toJSON x]
instance ToPairs (Attr Pack) where toPairs x = ["pack" .= toJSON x]
instance ToPairs (Attr Align) where toPairs x = ["align" .= toJSON x]
instance ToPairs (Attr Titles) where toPairs x = ["_titles" .= toJSON x]
instance ToPairs (Attr SelectedIndex) where toPairs x = ["selected_index" .= toJSON x]

-- | Store the value for a field, as an object parametrized by the Field. No verification is done
-- for these values.
(=::) :: SingI f => Sing f -> FieldType f -> Attr f
s =:: x = Attr { _value = Real x, _verify = return, _field = reflect s }

-- | If the number is in the range, return it. Otherwise raise the appropriate (over/under)flow
-- exception.
rangeCheck :: (Num a, Ord a) => (a, a) -> a -> IO a
rangeCheck (l, u) x
  | l <= x && x <= u = return x
  | l > x = Ex.throw Ex.Underflow
  | u < x = Ex.throw Ex.Overflow

-- | Store a numeric value, with verification mechanism for its range.
ranged :: (SingI f, Num (FieldType f), Ord (FieldType f))
       => Sing f -> (FieldType f, FieldType f) -> AttrVal (FieldType f) -> Attr f
ranged s range x = Attr x (rangeCheck range) (reflect s)

-- | Store a numeric value, with the invariant that it stays non-negative. The value set is set as a
-- dummy value if it's equal to zero.
(=:+) :: (SingI f, Num (FieldType f), CustomBounded (FieldType f), Ord (FieldType f))
      => Sing f -> FieldType f -> Attr f
s =:+ val = Attr ((if val == 0 then Dummy else Real) val) (rangeCheck (0, upperBound)) (reflect s)

-- | Get a field from a singleton
-- Adapted from: http://stackoverflow.com/a/28033250/2388535
reflect :: forall (f :: Field). (SingI f, SingKind ('KProxy :: KProxy Field)) => Sing f -> Field
reflect = fromSing

-- | A record representing an object of the Widget class from IPython
defaultWidget :: FieldType ViewName -> Rec Attr WidgetClass
defaultWidget viewName = (SViewModule =:: "")
                      :& (SViewName =:: viewName)
                      :& (SMsgThrottle =:+ 3)
                      :& (SVersion =:: 0)
                      :& (SDisplayHandler =:: return ())
                      :& RNil

-- | A record representing an object of the DOMWidget class from IPython
defaultDOMWidget :: FieldType ViewName -> Rec Attr DOMWidgetClass
defaultDOMWidget viewName = defaultWidget viewName <+> domAttrs
  where domAttrs = (SVisible =:: True)
                :& (SCSS =:: [])
                :& (SDOMClasses =:: [])
                :& (SWidth =:+ 0)
                :& (SHeight =:+ 0)
                :& (SPadding =:+ 0)
                :& (SMargin =:+ 0)
                :& (SColor =:: "")
                :& (SBackgroundColor =:: "")
                :& (SBorderColor =:: "")
                :& (SBorderWidth =:+ 0)
                :& (SBorderRadius =:+ 0)
                :& (SBorderStyle =:: DefaultBorder)
                :& (SFontStyle =:: DefaultFont)
                :& (SFontWeight =:: DefaultWeight)
                :& (SFontSize =:+ 0)
                :& (SFontFamily =:: "")
                :& RNil

-- | A record representing a widget of the _String class from IPython
defaultStringWidget :: FieldType ViewName -> Rec Attr StringClass
defaultStringWidget viewName = defaultDOMWidget viewName <+> strAttrs
  where strAttrs = (SStringValue =:: "")
                :& (SDisabled =:: False)
                :& (SDescription =:: "")
                :& (SPlaceholder =:: "")
                :& RNil

-- | A record representing a widget of the _Bool class from IPython
defaultBoolWidget :: FieldType ViewName -> Rec Attr BoolClass
defaultBoolWidget viewName = defaultDOMWidget viewName <+> boolAttrs
  where boolAttrs = (SBoolValue =:: False)
                 :& (SDisabled =:: False)
                 :& (SDescription =:: "")
                 :& (SChangeHandler =:: return ())
                 :& RNil

-- | A record representing a widget of the _Selection class from IPython
defaultSelectionWidget :: FieldType ViewName -> Rec Attr SelectionClass
defaultSelectionWidget viewName = defaultDOMWidget viewName <+> selectionAttrs
  where selectionAttrs = (SOptions =:: OptionLabels [])
                      :& (SSelectedValue =:: "")
                      :& (SSelectedLabel =:: "")
                      :& (SDisabled =:: False)
                      :& (SDescription =:: "")
                      :& (SSelectionHandler =:: return ())
                      :& RNil

-- | A record representing a widget of the _MultipleSelection class from IPython
defaultMultipleSelectionWidget :: FieldType ViewName -> Rec Attr MultipleSelectionClass
defaultMultipleSelectionWidget viewName = defaultDOMWidget viewName <+> mulSelAttrs
  where mulSelAttrs = (SOptions =:: OptionLabels [])
                   :& (SSelectedLabels =:: [])
                   :& (SSelectedValues =:: [])
                   :& (SDisabled =:: False)
                   :& (SDescription =:: "")
                   :& (SSelectionHandler =:: return ())
                   :& RNil

-- | A record representing a widget of the _Int class from IPython
defaultIntWidget :: FieldType ViewName -> Rec Attr IntClass
defaultIntWidget viewName = defaultDOMWidget viewName <+> intAttrs
  where intAttrs = (SIntValue =:: 0)
                :& (SDisabled =:: False)
                :& (SDescription =:: "")
                :& (SChangeHandler =:: return ())
                :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultBoundedIntWidget :: FieldType ViewName -> Rec Attr BoundedIntClass
defaultBoundedIntWidget viewName = defaultIntWidget viewName <+> boundedIntAttrs
  where boundedIntAttrs = (SStepInt =:: 1)
                       :& (SMinInt =:: 0)
                       :& (SMaxInt =:: 100)
                       :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultIntRangeWidget :: FieldType ViewName -> Rec Attr IntRangeClass
defaultIntRangeWidget viewName = defaultIntWidget viewName <+> rangeAttrs
  where rangeAttrs = (SIntPairValue =:: (25, 75))
                  :& (SLowerInt =:: 0)
                  :& (SUpperInt =:: 100)
                  :& RNil

-- | A record representing a widget of the _BoundedIntRange class from IPython
defaultBoundedIntRangeWidget :: FieldType ViewName -> Rec Attr BoundedIntRangeClass
defaultBoundedIntRangeWidget viewName = defaultIntRangeWidget viewName <+> boundedIntRangeAttrs
  where boundedIntRangeAttrs = (SStepInt =:+ 1)
                            :& (SMinInt =:: 0)
                            :& (SMaxInt =:: 100)
                            :& RNil

-- | A record representing a widget of the _Float class from IPython
defaultFloatWidget :: FieldType ViewName -> Rec Attr FloatClass
defaultFloatWidget viewName = defaultDOMWidget viewName <+> intAttrs
  where intAttrs = (SFloatValue =:: 0)
                :& (SDisabled =:: False)
                :& (SDescription =:: "")
                :& (SChangeHandler =:: return ())
                :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultBoundedFloatWidget :: FieldType ViewName -> Rec Attr BoundedFloatClass
defaultBoundedFloatWidget viewName = defaultFloatWidget viewName <+> boundedFloatAttrs
  where boundedFloatAttrs = (SStepFloat =:+ 1)
                       :& (SMinFloat =:: 0)
                       :& (SMaxFloat =:: 100)
                       :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultFloatRangeWidget :: FieldType ViewName -> Rec Attr FloatRangeClass
defaultFloatRangeWidget viewName = defaultFloatWidget viewName <+> rangeAttrs
  where rangeAttrs = (SFloatPairValue =:: (25, 75))
                  :& (SLowerFloat =:: 0)
                  :& (SUpperFloat =:: 100)
                  :& RNil

-- | A record representing a widget of the _BoundedFloatRange class from IPython
defaultBoundedFloatRangeWidget :: FieldType ViewName -> Rec Attr BoundedFloatRangeClass
defaultBoundedFloatRangeWidget viewName = defaultFloatRangeWidget viewName <+> boundedFloatRangeAttrs
  where boundedFloatRangeAttrs = (SStepFloat =:+ 1)
                              :& (SMinFloat =:: 0)
                              :& (SMaxFloat =:: 100)
                              :& RNil

-- | A record representing a widget of the _Box class from IPython
defaultBoxWidget :: FieldType ViewName -> Rec Attr BoxClass
defaultBoxWidget viewName = defaultDOMWidget viewName <+> boxAttrs
  where boxAttrs = (SChildren =:: [])
                :& (SOverflowX =:: DefaultOverflow)
                :& (SOverflowY =:: DefaultOverflow)
                :& (SBoxStyle =:: DefaultBox)
                :& RNil

-- | A record representing a widget of the _SelectionContainer class from IPython
defaultSelectionContainerWidget :: FieldType ViewName -> Rec Attr SelectionContainerClass
defaultSelectionContainerWidget viewName = defaultBoxWidget viewName <+> selAttrs
  where selAttrs = (STitles =:: [])
                :& (SSelectedIndex =:: 0)
                :& (SChangeHandler =:: return ())
                :& RNil

newtype WidgetState w = WidgetState { _getState :: Rec Attr (WidgetFields w) }

-- All records with ToPair instances for their Attrs will automatically have a toJSON instance now.
instance RecAll Attr (WidgetFields w) ToPairs => ToJSON (WidgetState w) where
  toJSON record =
    object
    . concat
    . recordToList
    . rmap (\(Compose (Dict x)) -> Const $ toPairs x)
    $ reifyConstraint (Proxy :: Proxy ToPairs) $ _getState record

data IPythonWidget (w :: WidgetType) = IPythonWidget { uuid :: UUID, state :: IORef (WidgetState w) }

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

properties :: IPythonWidget w -> IO [Field]
properties widget = do
  st <- readIORef $ state widget
  let convert :: Attr f -> Const Field f
      convert attr = Const { getConst = _field attr }
  return $ recordToList . rmap convert . _getState $ st

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

triggerChange :: (ChangeHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerChange = triggerEvent SChangeHandler

triggerClick :: (ClickHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerClick = triggerEvent SClickHandler

triggerSelection :: (SelectionHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerSelection = triggerEvent SSelectionHandler

triggerSubmit :: (SubmitHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerSubmit = triggerEvent SSubmitHandler

triggerDisplay :: (DisplayHandler ∈ WidgetFields w) => IPythonWidget w -> IO ()
triggerDisplay = triggerEvent SDisplayHandler
