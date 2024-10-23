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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}

-- | This module houses all the type-trickery needed to make widgets happen.
--
-- All widgets have a data type without a constructor defined later in this file. Some
-- fields/attributes/properties as defined by the 'WidgetFields' type-family.
--
-- Each widget field corresponds to a concrete haskell type, as given by the 'FieldType'
-- type-family.
--
-- Vinyl records are used to wrap together widget fields into a single 'WidgetState'.
--
-- Users pass types by visible type application.
--
-- A vinyl record is represented as @Rec f ts@, which means that a record is a list of @f x@, where
-- @x@ is a type present in the type-level list @ts@. Thus a 'WidgetState' is essentially a list of
-- field properties wrapped together with the corresponding field name. See ('=::') for more.
--
-- The properties function can be used to view all the fields associated with a widget object.
--
-- Attributes are represented by the @Attr@ data type, which holds the value of a field and a
-- function to verify validity of changes to the value.
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
--
-- Widgets and fields are open. You can create a custom widget by implementing WidgetFields and new
-- fields by implementing FieldType and ToKey.
module IHaskell.Display.Widgets.Types where

import           Control.Applicative ((<$>))
import qualified Control.Exception as Ex
import           Control.Monad (unless, join, when, void,mzero)
import           Data.IORef (IORef, readIORef, modifyIORef)
import           Data.String
import           Data.Text (Text, pack)
import           Data.Typeable (Typeable, TypeRep, typeOf, typeRep, Proxy(..))
import           System.IO.Error
import           System.Posix.IO
import           Text.Printf (printf)
import           GHC.Exts (Constraint)
import           GHC.TypeLits

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
import           Data.Vinyl.Lens (rget, rput, RElem)
import           Data.Vinyl.TypeLevel (RecAll, RIndex)

import           Data.Text.Lazy (unpack)
import           Data.Text.Lazy.Encoding

import           GHC.IO.Exception

import           IHaskell.Eval.Widgets (widgetSendUpdate, widgetSendView)
import           IHaskell.Display (IHaskellWidget(..), IHaskellDisplay(..), Display(..), widgetdisplay, base64)
import           IHaskell.IPython.Types (StreamType(..))
import           IHaskell.IPython.Message.UUID

import           IHaskell.Display.Widgets.Singletons (ToKey, toKey, HasKey)
import           IHaskell.Display.Widgets.Singletons
import qualified IHaskell.Display.Widgets.Singletons as S
import           IHaskell.Display.Widgets.Common

type RElemOf r rs = RElem r rs (RIndex r rs) 

-- | F for field. Nicer than Proxy.
-- Used only in infix functions since @f := val is not allowed.
data F a = F

-- Define the type family for appending two type-level lists
type family (:++) (xs :: [a]) (ys :: [a]) :: [a] where
    '[] :++ ys = ys
    (x ': xs) :++ ys = x ': (xs :++ ys)

-- Classes from IPython's widget hierarchy. Defined as such to reduce code duplication.
type CoreWidgetClass = '[S.ViewModule, S.ViewModuleVersion, S.ModelModule, S.ModelModuleVersion ]

type DOMWidgetClass = '[S.ModelName, S.ViewName, S.DOMClasses, S.Tabbable, S.Tooltip, S.Layout, S.DisplayHandler]

type StyleWidgetClass = '[S.ModelName, S.ViewName] :++ CoreWidgetClass

type DescriptionWidgetClass = CoreWidgetClass :++ DOMWidgetClass :++ '[S.Description,S.DescriptionAllowHtml,S.Style]

type StringClass = DescriptionWidgetClass :++ '[S.StringValue, S.Placeholder]

type TextClass = StringClass :++ '[ S.Disabled, S.ContinuousUpdate, S.SubmitHandler, S.ChangeHandler]

type BoolClass = DescriptionWidgetClass :++ '[S.BoolValue, S.Disabled, S.ChangeHandler]

type SelectionClass = DescriptionWidgetClass :++ '[S.OptionsLabels, S.OptionalIndex, S.Disabled, S.SelectionHandler]

type SelectionNonemptyClass = DescriptionWidgetClass :++ '[S.OptionsLabels, S.Index, S.Disabled, S.SelectionHandler]

type MultipleSelectionClass = DescriptionWidgetClass :++ '[S.OptionsLabels, S.Indices, S.Disabled, S.SelectionHandler]

type IntClass = DescriptionWidgetClass :++ '[ S.IntValue, S.ChangeHandler ]

type BoundedIntClass = IntClass :++ '[S.MaxInt, S.MinInt]

type IntRangeClass = IntClass :++ '[S.IntPairValue, S.LowerInt, S.UpperInt]

type BoundedIntRangeClass = IntRangeClass :++ '[S.MaxInt, S.MinInt]

type FloatClass = DescriptionWidgetClass :++ '[ S.FloatValue, S.ChangeHandler ]

type BoundedFloatClass = FloatClass :++ '[S.MinFloat, S.MaxFloat]

type BoundedLogFloatClass = FloatClass :++ '[ S.MinFloat, S.MaxFloat, S.BaseFloat ]

type FloatRangeClass = FloatClass :++ '[ S.FloatPairValue ]

type BoundedFloatRangeClass = FloatRangeClass :++ '[S.StepFloat, S.MinFloat, S.MaxFloat]

type BoxClass = CoreWidgetClass :++ DOMWidgetClass :++ '[S.Children, S.BoxStyle]

type SelectionContainerClass = BoxClass :++ '[S.Titles, S.SelectedIndex, S.ChangeHandler]

type MediaClass = CoreWidgetClass :++ DOMWidgetClass :++ '[ S.BSValue ]

type DescriptionStyleClass = StyleWidgetClass :++ '[ S.DescriptionWidth ]

type LinkClass = CoreWidgetClass :++ '[S.ModelName, S.Target, S.Source]

-- Types associated with Fields.
type family FieldType f :: *

type instance FieldType S.ViewModule = Text
type instance FieldType S.ViewModuleVersion = Text
type instance FieldType S.ViewName = Text
type instance FieldType S.ModelModule = Text
type instance FieldType S.ModelModuleVersion = Text
type instance FieldType S.ModelName = Text
type instance FieldType S.Layout = IPythonWidget LayoutType
type instance FieldType S.DisplayHandler = IO ()
type instance FieldType S.DOMClasses = [Text]
type instance FieldType S.Width = PixCount
type instance FieldType S.Height = PixCount
type instance FieldType S.Description = Text
type instance FieldType S.DescriptionAllowHtml = Maybe Bool
type instance FieldType S.ClickHandler = IO ()
type instance FieldType S.SubmitHandler = IO ()
type instance FieldType S.Disabled = Bool
type instance FieldType S.StringValue = Text
type instance FieldType S.Placeholder = Text
type instance FieldType S.Tabbable = Maybe Bool
type instance FieldType S.Tooltip = Maybe Text
type instance FieldType S.Icon = Text
type instance FieldType S.ButtonStyleField = ButtonStyleValue
type instance FieldType S.BSValue = JSONByteString
type instance FieldType S.ImageFormat = ImageFormatValue
type instance FieldType S.BoolValue = Bool
type instance FieldType S.OptionsLabels = [Text]
type instance FieldType S.Index = Integer
type instance FieldType S.OptionalIndex = Maybe Integer
type instance FieldType S.SelectionHandler = IO ()
type instance FieldType S.Tooltips = [Text]
type instance FieldType S.Icons = [Text]
type instance FieldType S.Indices = [Integer]
type instance FieldType S.IntValue = Integer
type instance FieldType S.StepInt = Maybe Integer
type instance FieldType S.MinInt = Integer
type instance FieldType S.MaxInt = Integer
type instance FieldType S.LowerInt = Integer
type instance FieldType S.UpperInt = Integer
type instance FieldType S.IntPairValue = (Integer, Integer)
type instance FieldType S.Orientation = OrientationValue
type instance FieldType S.BaseFloat = Double
type instance FieldType S.ReadOut = Bool
type instance FieldType S.ReadOutFormat = Text
type instance FieldType S.BarStyle = BarStyleValue
type instance FieldType S.FloatValue = Double
type instance FieldType S.StepFloat = Maybe Double
type instance FieldType S.MinFloat = Double
type instance FieldType S.MaxFloat = Double
type instance FieldType S.LowerFloat = Double
type instance FieldType S.UpperFloat = Double
type instance FieldType S.FloatPairValue = (Double, Double)
type instance FieldType S.ChangeHandler = IO ()
type instance FieldType S.Children = [ChildWidget]
type instance FieldType S.BoxStyle = BoxStyleValue
type instance FieldType S.Titles = [Text]
type instance FieldType S.SelectedIndex = Maybe Integer
type instance FieldType S.ReadOutMsg = Text
type instance FieldType S.Indent = Bool
type instance FieldType S.ContinuousUpdate = Bool
type instance FieldType S.Rows = Maybe Integer
type instance FieldType S.AudioFormat = AudioFormatValue
type instance FieldType S.VideoFormat = VideoFormatValue
type instance FieldType S.AutoPlay = Bool
type instance FieldType S.Loop = Bool
type instance FieldType S.Controls = Bool
type instance FieldType S.Options = [Text]
type instance FieldType S.EnsureOption = Bool
type instance FieldType S.Playing = Bool
type instance FieldType S.Repeat = Bool
type instance FieldType S.Interval = Integer
type instance FieldType S.ShowRepeat = Bool
type instance FieldType S.Concise = Bool
type instance FieldType S.DateValue = Date
type instance FieldType S.Pressed = Bool
type instance FieldType S.Name = Text
type instance FieldType S.Mapping = Text
type instance FieldType S.Connected = Bool
type instance FieldType S.Timestamp = Double
type instance FieldType S.Buttons = [IPythonWidget ControllerButtonType]
type instance FieldType S.Axes = [IPythonWidget ControllerAxisType]
type instance FieldType S.ButtonColor = Maybe String
type instance FieldType S.FontFamily = Maybe String
type instance FieldType S.FontSize = Maybe String
type instance FieldType S.FontStyle = Maybe String
type instance FieldType S.FontVariant = Maybe String
type instance FieldType S.FontWeight = FontWeightValue
type instance FieldType S.TextColor = Maybe String
type instance FieldType S.TextDecoration = Maybe String
type instance FieldType S.DescriptionWidth = String
type instance FieldType S.BarColor = Maybe String
type instance FieldType S.HandleColor = Maybe String
type instance FieldType S.ButtonWidth = String
type instance FieldType S.Target = WidgetFieldPair
type instance FieldType S.Source = WidgetFieldPair
type instance FieldType S.MsgID = Text
type instance FieldType S.Outputs = [OutputMsg]
type instance FieldType S.Style = StyleWidget

-- | Can be used to put different widgets in a list. Useful for dealing with children widgets.
data ChildWidget = forall w. RecAll Attr (WidgetFields w) ToPairs => ChildWidget (IPythonWidget w)

-- | Can be used to put different styles in a same FieldType.
data StyleWidget = forall w. RecAll Attr (WidgetFields w) ToPairs => StyleWidget (IPythonWidget w)

instance ToJSON (IPythonWidget w) where
  toJSON x = toJSON . pack $ "IPY_MODEL_" ++ uuidToString (uuid x)

instance ToJSON ChildWidget where
  toJSON (ChildWidget x) = toJSON x

instance ToJSON StyleWidget where
  toJSON (StyleWidget x) = toJSON x

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

-- | This type only fits if the field is among the widget's fields, and it has a key
data WidgetFieldPair = forall w f. (RElemOf f (WidgetFields w), ToKey f, HasKey f ~ 'True, RecAll Attr (WidgetFields w) ToPairs)
                     => WidgetFieldPair (IPythonWidget w) (Proxy f) | EmptyWT

instance ToJSON WidgetFieldPair where
  toJSON EmptyWT = Null
  toJSON (WidgetFieldPair w (Proxy :: Proxy f)) = toJSON [toJSON w, toJSON $ pack $ toKey @f]

-- Different types of widgets. Every widget in IPython has a corresponding widget type.
data ButtonType
data ColorPickerType
data DatePickerType
data AudioType
data ImageType
data VideoType
data OutputType
data HTMLType
data HTMLMathType
data ComboboxType
data LabelType
data PasswordType
data TextType
data TextAreaType
data CheckBoxType
data ToggleButtonType
data ValidType
data DropdownType
data RadioButtonsType
data SelectType
data SelectionSliderType
data SelectionRangeSliderType
data ToggleButtonsType
data SelectMultipleType
data IntTextType
data BoundedIntTextType
data IntSliderType
data PlayType
data IntProgressType
data IntRangeSliderType
data FloatTextType
data BoundedFloatTextType
data FloatSliderType
data FloatLogSliderType
data FloatProgressType
data FloatRangeSliderType
data BoxType
data GridBoxType
data HBoxType
data VBoxType
data AccordionType
data TabType
data StackedType
data ControllerButtonType
data ControllerAxisType
data ControllerType
data LinkType
data DirectionalLinkType
data LayoutType
data ButtonStyleType
data DescriptionStyleType
data ProgressStyleType
data SliderStyleType
data ToggleButtonsStyleType

-- Fields associated with a widget

type family AllToKey (xs :: [*]) :: Constraint where
  AllToKey '[]       = ()
  AllToKey (x ': xs) = (ToKey x, Typeable x, AllToKey xs)

type family WidgetFields w :: [*]
type instance WidgetFields ButtonType =
                DescriptionWidgetClass :++
                  [S.Disabled, S.Icon, S.ButtonStyleField,S.ClickHandler]
type instance WidgetFields ColorPickerType =
                DescriptionWidgetClass :++
                  [S.StringValue, S.Concise, S.Disabled, S.ChangeHandler]
type instance WidgetFields DatePickerType =
                DescriptionWidgetClass :++
                  [S.DateValue, S.Disabled, S.ChangeHandler]

type instance WidgetFields AudioType =
                MediaClass :++ [S.AudioFormat, S.AutoPlay, S.Loop, S.Controls]
type instance WidgetFields ImageType =
                MediaClass :++ [S.ImageFormat, S.Width, S.Height]
type instance WidgetFields VideoType =
                MediaClass :++ [S.VideoFormat, S.Width, S.Height, S.AutoPlay, S.Loop, S.Controls]

type instance WidgetFields OutputType = DOMWidgetClass :++ [S.ViewModule,S.ModelModule,S.ViewModuleVersion,S.ModelModuleVersion,S.MsgID,S.Outputs]
type instance WidgetFields HTMLType = StringClass
type instance WidgetFields HTMLMathType = StringClass
type instance WidgetFields ComboboxType = TextClass :++ [ S.Options, S.EnsureOption ]
type instance WidgetFields LabelType = StringClass
type instance WidgetFields PasswordType = TextClass
type instance WidgetFields TextType = TextClass

-- Type level lists with a single element need both the list and the
-- constructor ticked, and a space between the open square bracket and
-- the first constructor. See https://ghc.haskell.org/trac/ghc/ticket/15601
type instance WidgetFields TextAreaType =
                StringClass :++
                  [ S.Rows, S.Disabled, S.ContinuousUpdate, S.ChangeHandler]

type instance WidgetFields CheckBoxType = BoolClass :++ '[ S.Indent ]
type instance WidgetFields ToggleButtonType = BoolClass :++ [S.Icon, S.ButtonStyleField]
type instance WidgetFields ValidType = BoolClass :++ '[ S.ReadOutMsg ]
type instance WidgetFields DropdownType = SelectionClass
type instance WidgetFields RadioButtonsType = SelectionClass
type instance WidgetFields SelectType = SelectionClass :++ '[ S.Rows ]
type instance WidgetFields SelectionSliderType = SelectionNonemptyClass :++ '[ S.Orientation, S.ReadOut, S.ContinuousUpdate ]
type instance WidgetFields SelectionRangeSliderType = MultipleSelectionClass :++ '[ S.Orientation, S.ReadOut, S.ContinuousUpdate ]
type instance WidgetFields ToggleButtonsType =
                SelectionClass :++ [S.Tooltips, S.Icons, S.ButtonStyleField]
type instance WidgetFields SelectMultipleType = MultipleSelectionClass :++ '[ S.Rows ]
type instance WidgetFields IntTextType = IntClass :++ [ S.Disabled, S.ContinuousUpdate, S.StepInt ]
type instance WidgetFields BoundedIntTextType = BoundedIntClass :++ [ S.Disabled, S.ContinuousUpdate, S.StepInt ]
type instance WidgetFields IntSliderType =
                BoundedIntClass :++
                  [ S.StepInt, S.Orientation, S.ReadOut, S.ReadOutFormat, S.ContinuousUpdate, S.Disabled ]
type instance WidgetFields PlayType =
                BoundedIntClass :++
                  [ S.Playing, S.Repeat, S.Interval, S.StepInt, S.Disabled, S.ShowRepeat ]
type instance WidgetFields IntProgressType =
                BoundedIntClass :++ [S.Orientation, S.BarStyle]
type instance WidgetFields IntRangeSliderType =
                BoundedIntRangeClass :++
                  [S.StepInt, S.Orientation, S.ReadOut, S.ReadOutFormat, S.ContinuousUpdate, S.Disabled ]
type instance WidgetFields FloatTextType = FloatClass :++ '[ S.Disabled, S.ContinuousUpdate, S.StepFloat ]
type instance WidgetFields BoundedFloatTextType = BoundedFloatClass :++ '[ S.Disabled, S.ContinuousUpdate, S.StepFloat ]
type instance WidgetFields FloatSliderType =
                BoundedFloatClass :++
                  [S.StepFloat, S.Orientation, S.ReadOut, S.ReadOutFormat, S.ContinuousUpdate, S.Disabled ]
type instance WidgetFields FloatLogSliderType =
                BoundedLogFloatClass :++
                  [S.StepFloat, S.Orientation, S.ReadOut, S.ReadOutFormat, S.ContinuousUpdate, S.Disabled, S.BaseFloat]
type instance WidgetFields FloatProgressType =
                BoundedFloatClass :++ [S.Orientation, S.BarStyle]
type instance WidgetFields FloatRangeSliderType =
                BoundedFloatRangeClass :++
                  [S.StepFloat, S.Orientation, S.ReadOut, S.ReadOutFormat, S.ContinuousUpdate, S.Disabled ]
type instance WidgetFields BoxType = BoxClass
type instance WidgetFields GridBoxType = BoxClass
type instance WidgetFields HBoxType = BoxClass
type instance WidgetFields VBoxType = BoxClass
type instance WidgetFields AccordionType = SelectionContainerClass
type instance WidgetFields TabType = SelectionContainerClass
type instance WidgetFields StackedType = SelectionContainerClass
type instance WidgetFields ControllerType =
  CoreWidgetClass :++ DOMWidgetClass :++
    [S.Index, S.Name, S.Mapping, S.Connected, S.Timestamp, S.Buttons, S.Axes, S.ChangeHandler ]
type instance WidgetFields ControllerAxisType = CoreWidgetClass :++ DOMWidgetClass :++ '[ S.FloatValue, S.ChangeHandler ]
type instance WidgetFields ControllerButtonType = CoreWidgetClass :++ DOMWidgetClass :++ [ S.FloatValue, S.Pressed, S.ChangeHandler ]
type instance WidgetFields LinkType = LinkClass
type instance WidgetFields DirectionalLinkType = LinkClass
type instance WidgetFields ButtonStyleType = StyleWidgetClass :++ [S.ButtonColor, S.FontFamily, S.FontSize, S.FontStyle, S.FontVariant, S.FontWeight, S.TextColor, S.TextDecoration]
type instance WidgetFields DescriptionStyleType = DescriptionStyleClass
type instance WidgetFields ProgressStyleType = DescriptionStyleClass :++ '[ S.BarColor ]
type instance WidgetFields SliderStyleType = DescriptionStyleClass :++ '[ S.HandleColor ]
type instance WidgetFields ToggleButtonsStyleType = DescriptionStyleClass :++ [S.ButtonWidth,S.FontWeight]

-- Wrapper around a field's value. A dummy value is sent as an empty string to the frontend.
data AttrVal a = Dummy a
               | Real a

unwrap :: AttrVal a -> a
unwrap (Dummy x) = x
unwrap (Real x) = x

-- Wrapper around a field.
data Attr f where
  Attr :: (Typeable (FieldType f), ToKey f)
       => { _value :: AttrVal (FieldType f)
          , _verify :: FieldType f -> IO (FieldType f)
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

-- From https://stackoverflow.com/questions/68648670/duplicate-instance-declaration-using-haskell-singletons
instance (ToKey f, ToPairs' (HasKey f) f) => ToPairs (Attr f) where
  toPairs = toPairs'

class (ToKey a, hk ~ HasKey a) => ToPairs' hk a where
  toPairs' :: Attr a -> [Pair]

instance (ToKey f, HasKey f ~ 'False) => ToPairs' 'False f where
  toPairs' _ = []

instance (ToKey f, ToJSON (FieldType f), HasKey f ~ 'True) => ToPairs' 'True f where
  toPairs' x = [ fromString (toKey @f) .= toJSON x ]

newtype JSONByteString = JSONByteString ByteString
  deriving (Eq,Ord)

instance ToJSON JSONByteString where
  toJSON (JSONByteString x) = toJSON $ base64 x

instance IsString JSONByteString where
  fromString = JSONByteString . fromString

-- | Store the value for a field, as an object parametrized by the Field. No verification is done
-- for these values.
(=::) :: forall f. (Typeable (FieldType f), ToKey f) => F f -> FieldType f -> Attr f
F =:: x = Attr { _value = Real x, _verify = return, _ro = False }

-- | Store the value for a field, with a custom verification
(=:.) :: forall f. (Typeable (FieldType f), ToKey f) => F f -> (FieldType f, FieldType f -> IO (FieldType f) ) -> Attr f
F =:. (x,v) = Attr { _value = Real x, _verify = v, _ro = False }

-- | Store the value for a field, making it read only from the frontend
(=:!) :: forall f. (Typeable (FieldType f), ToKey f) => F f -> FieldType f -> Attr f
F =:! x = Attr { _value = Real x, _verify = return, _ro = True}

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
ranged :: forall f. (ToKey f, Num (FieldType f), Ord (FieldType f), Typeable (FieldType f))
       => (FieldType f, FieldType f) -> AttrVal (FieldType f) -> Attr f
ranged range x = Attr x (rangeCheck range) False

-- | Store a numeric value, with the invariant that it stays non-negative. The value set is set as a
-- dummy value if it's equal to zero.
(=:+) :: forall f. (Num (FieldType f), CustomBounded (FieldType f), Ord (FieldType f), Typeable (FieldType f), ToKey f)
      => F f -> FieldType f -> Attr f
F =:+ val = Attr
              ((if val == 0
                  then Dummy
                  else Real)
                 val)
              (rangeCheck (0, upperBound))
              False

-- | A record representing a Widget class from IPython from the controls modules
defaultCoreWidget :: Rec Attr CoreWidgetClass
defaultCoreWidget = (F @ViewModule =:! "@jupyter-widgets/controls")
                    :& (F @ViewModuleVersion =:! "2.0.0")
                    :& (F @ModelModule =:! "@jupyter-widgets/controls")
                    :& (F @ModelModuleVersion =:! "2.0.0")
                    :& RNil

-- | A record representing an object of the DOMWidget class from IPython
defaultDOMWidget :: FieldType S.ViewName -> FieldType S.ModelName -> IPythonWidget LayoutType -> Rec Attr DOMWidgetClass
defaultDOMWidget viewName modelName layout = (F @ModelName =:! modelName)
                                      :& (F @ViewName =:! viewName)
                                      :& (F @DOMClasses =:: [])
                                      :& (F @Tabbable =:: Nothing)
                                      :& (F @Tooltip =:: Nothing)
                                      :& (F @Layout =:: layout)
                                      :& (F @DisplayHandler =:: return ())
                                      :& RNil

-- | A record representing an object of the DescriptionWidget class from IPython
defaultDescriptionWidget :: FieldType S.ViewName
                         -> FieldType S.ModelName
                         -> IPythonWidget LayoutType
                         -> StyleWidget
                         -> Rec Attr DescriptionWidgetClass
defaultDescriptionWidget v m l d = defaultCoreWidget <+> defaultDOMWidget v m l <+> descriptionAttrs
  where
    descriptionAttrs = (F @Description =:: "")
                       :& (F @DescriptionAllowHtml =:: Nothing)
                       :& (F @Style =:: d)
                       :& RNil

-- | A record representing a widget of the _String class from IPython
defaultStringWidget :: FieldType S.ViewName
                    -> FieldType S.ModelName
                    -> IPythonWidget LayoutType
                    -> StyleWidget
                    -> Rec Attr StringClass
defaultStringWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> strAttrs
  where
    strAttrs = (F @StringValue =:: "")
               :& (F @Placeholder =:: "")
               :& RNil

-- | A record representing a widget of the Text class from IPython
defaultTextWidget :: FieldType S.ViewName
                  -> FieldType S.ModelName
                  -> IPythonWidget LayoutType
                  -> StyleWidget
                  -> Rec Attr TextClass
defaultTextWidget viewName modelName l d = defaultStringWidget viewName modelName l d <+> txtAttrs
  where
    txtAttrs = (F @Disabled =:: False)
               :& (F @ContinuousUpdate =:: True)
               :& (F @SubmitHandler =:: return ())
               :& (F @ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _Bool class from IPython
defaultBoolWidget :: FieldType S.ViewName
                  -> FieldType S.ModelName
                  -> IPythonWidget LayoutType
                  -> StyleWidget
                  -> Rec Attr BoolClass
defaultBoolWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> boolAttrs
  where
    boolAttrs = (F @BoolValue =:: False)
                :& (F @Disabled =:: False)
                :& (F @ChangeHandler =:: return ())
                :& RNil

-- | A record representing a widget of the _Selection class from IPython
defaultSelectionWidget :: FieldType S.ViewName
                       -> FieldType S.ModelName
                       -> IPythonWidget LayoutType
                       -> StyleWidget
                       -> Rec Attr SelectionClass
defaultSelectionWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> selectionAttrs
  where
    selectionAttrs = (F @OptionsLabels =:: [])
                     :& (F @OptionalIndex =:: Nothing)
                     :& (F @Disabled =:: False)
                     :& (F @SelectionHandler =:: return ())
                     :& RNil

-- | A record representing a widget of the _SelectionNonempty class from IPython
defaultSelectionNonemptyWidget :: FieldType S.ViewName
                               -> FieldType S.ModelName
                               -> IPythonWidget LayoutType
                               -> StyleWidget
                               -> Rec Attr SelectionNonemptyClass
defaultSelectionNonemptyWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> selectionAttrs
  where
    selectionAttrs = (F @OptionsLabels =:: [])
                     :& (F @Index =:: 0)
                     :& (F @Disabled =:: False)
                     :& (F @SelectionHandler =:: return ())
                     :& RNil

-- | A record representing a widget of the _MultipleSelection class from IPython
defaultMultipleSelectionWidget :: FieldType S.ViewName
                               -> FieldType S.ModelName
                               -> IPythonWidget LayoutType
                               -> StyleWidget
                               -> Rec Attr MultipleSelectionClass
defaultMultipleSelectionWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> mulSelAttrs
  where
    mulSelAttrs = (F @OptionsLabels =:: [])
                  :& (F @Indices =:: [])
                  :& (F @Disabled =:: False)
                  :& (F @SelectionHandler =:: return ())
                  :& RNil

-- | A record representing a widget of the _Int class from IPython
defaultIntWidget :: FieldType S.ViewName
                 -> FieldType S.ModelName
                 -> IPythonWidget LayoutType
                 -> StyleWidget
                 -> Rec Attr IntClass
defaultIntWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> intAttrs
  where
    intAttrs = (F @IntValue =:: 0)
               :& (F @ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultBoundedIntWidget :: FieldType S.ViewName
                        -> FieldType S.ModelName
                        -> IPythonWidget LayoutType
                        -> StyleWidget
                        -> Rec Attr BoundedIntClass
defaultBoundedIntWidget viewName modelName l d = defaultIntWidget viewName modelName l d <+> boundedIntAttrs
  where
    boundedIntAttrs = (F @MaxInt =:: 100)
                      :& (F @MinInt =:: 0)
                      :& RNil

-- | A record representing a widget of the _BoundedInt class from IPython
defaultIntRangeWidget :: FieldType S.ViewName
                      -> FieldType S.ModelName
                      -> IPythonWidget LayoutType
                      -> StyleWidget
                      -> Rec Attr IntRangeClass
defaultIntRangeWidget viewName modelName l d = defaultIntWidget viewName modelName l d <+> rangeAttrs
  where
    rangeAttrs = (F @IntPairValue =:: (25, 75))
                 :& (F @LowerInt =:: 0)
                 :& (F @UpperInt =:: 100)
                 :& RNil

-- | A record representing a widget of the _BoundedIntRange class from IPython
defaultBoundedIntRangeWidget :: FieldType S.ViewName
                             -> FieldType S.ModelName
                             -> IPythonWidget LayoutType
                             -> StyleWidget
                             -> Rec Attr BoundedIntRangeClass
defaultBoundedIntRangeWidget viewName modelName l d = defaultIntRangeWidget viewName modelName l d <+> boundedIntRangeAttrs
  where
    boundedIntRangeAttrs = (F @MaxInt =:: 100)
                           :& (F @MinInt =:: 0)
                           :& RNil

-- | A record representing a widget of the _Float class from IPython
defaultFloatWidget :: FieldType S.ViewName
                   -> FieldType S.ModelName
                   -> IPythonWidget LayoutType
                   -> StyleWidget
                   -> Rec Attr FloatClass
defaultFloatWidget viewName modelName l d = defaultDescriptionWidget viewName modelName l d <+> floatAttrs
  where
    floatAttrs = (F @FloatValue =:: 0.0)
               :& (F @ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultBoundedFloatWidget :: FieldType S.ViewName
                          -> FieldType S.ModelName
                          -> IPythonWidget LayoutType
                          -> StyleWidget
                          -> Rec Attr BoundedFloatClass
defaultBoundedFloatWidget viewName modelName l d = defaultFloatWidget viewName modelName l d <+> boundedFloatAttrs
  where
    boundedFloatAttrs = (F @MinFloat =:: 0)
                        :& (F @MaxFloat =:: 100)
                        :& RNil

-- | A record representing a widget of the _BoundedLogFloat class from IPython
defaultBoundedLogFloatWidget :: FieldType S.ViewName
                             -> FieldType S.ModelName
                             -> IPythonWidget LayoutType
                             -> StyleWidget
                             -> Rec Attr BoundedLogFloatClass
defaultBoundedLogFloatWidget viewName modelName l d = floatAttrs <+> boundedLogFloatAttrs
  where
    floatAttrs = rput (F @FloatValue =:: 1.0) $ defaultFloatWidget viewName modelName l d
    boundedLogFloatAttrs = (F @MinFloat =:: 0.0)
                           :& (F @MaxFloat =:: 4.0)
                           :& (F @BaseFloat =:: 10.0)
                           :& RNil

-- | A record representing a widget of the _BoundedFloat class from IPython
defaultFloatRangeWidget :: FieldType S.ViewName
                        -> FieldType S.ModelName
                        -> IPythonWidget LayoutType
                        -> StyleWidget
                        -> Rec Attr FloatRangeClass
defaultFloatRangeWidget viewName modelName l d = defaultFloatWidget viewName modelName l d <+> rangeAttrs
  where
    rangeAttrs = (F @FloatPairValue =:: (0.0, 1.0))
                 :& RNil

-- | A record representing a widget of the _BoundedFloatRange class from IPython
defaultBoundedFloatRangeWidget :: FieldType S.ViewName
                               -> FieldType S.ModelName
                               -> IPythonWidget LayoutType
                               -> StyleWidget
                               -> Rec Attr BoundedFloatRangeClass
defaultBoundedFloatRangeWidget viewName modelName l d = defaultFloatRangeWidget viewName modelName l d <+> boundedFloatRangeAttrs
  where
    boundedFloatRangeAttrs = (F @StepFloat =:: Just 1)
                             :& (F @MinFloat =:: 0)
                             :& (F @MaxFloat =:: 100)
                             :& RNil

-- | A record representing a widget of the _Box class from IPython
defaultBoxWidget :: FieldType S.ViewName -> FieldType S.ModelName -> IPythonWidget LayoutType -> Rec Attr BoxClass
defaultBoxWidget viewName modelName layout = defaultCoreWidget <+> defaultDOMWidget viewName modelName layout <+> intAttrs
  where
    intAttrs = (F @Children =:: [])
               :& (F @BoxStyle =:: DefaultBox)
               :& RNil

-- | A record representing a widget of the _SelectionContainer class from IPython
defaultSelectionContainerWidget :: FieldType S.ViewName -> FieldType S.ModelName -> IPythonWidget LayoutType -> Rec Attr SelectionContainerClass
defaultSelectionContainerWidget viewName modelName layout = defaultBoxWidget viewName modelName layout <+> selAttrs
  where
    selAttrs = (F @Titles =:: [])
               :& (F @SelectedIndex =:: Nothing)
               :& (F @ChangeHandler =:: return ())
               :& RNil

-- | A record representing a widget of the _Media class from IPython
defaultMediaWidget :: FieldType S.ViewName -> FieldType S.ModelName -> IPythonWidget LayoutType -> Rec Attr MediaClass
defaultMediaWidget viewName modelName layout = defaultCoreWidget <+> defaultDOMWidget viewName modelName layout <+> mediaAttrs
  where
    mediaAttrs = (F @BSValue =:: "")
                 :& RNil

defaultLinkWidget :: FieldType S.ModelName -> Rec Attr LinkClass
defaultLinkWidget modelName = defaultCoreWidget <+> linkAttrs
  where
    linkAttrs = (F @ModelName =:! modelName)
                :& (F @Target =:: EmptyWT)
                :& (F @Source =:: EmptyWT)
                :& RNil

-- | A record representing a widget of the Style class from IPython
defaultStyleWidget :: FieldType S.ModelName -> Rec Attr StyleWidgetClass
defaultStyleWidget modelName = (F @ModelName =:! modelName)
                              :& (F @ViewName =:! "StyleView")
                              :& (F @ViewModule =:! "@jupyter-widgets/base")
                              :& (F @ViewModuleVersion =:! "2.0.0")
                              :& (F @ModelModule =:! "@jupyter-widgets/controls")
                              :& (F @ModelModuleVersion =:! "2.0.0")
                              :& RNil

-- | A record representing a widget of the DescriptionStyle class from IPython
defaultDescriptionStyleWidget :: FieldType S.ModelName -> Rec Attr DescriptionStyleClass
defaultDescriptionStyleWidget modelName = defaultStyleWidget modelName <+> dstyle
  where
    dstyle = (F @DescriptionWidth =:: "")
            :& RNil

data WidgetState w where
  WidgetState :: AllToKey (WidgetFields w) => { _getState :: Rec Attr (WidgetFields w) } -> WidgetState w

-- All records with ToPair instances for their Attrs will automatically have a toJSON instance now.
instance (AllToKey (WidgetFields w), RecAll Attr (WidgetFields w) ToPairs) => ToJSON (WidgetState w) where
  toJSON record =
    object
    . concat
      . recordToList
        . rmap (\(Compose (Dict x)) -> Const $ toPairs x) $ reifyConstraint (Proxy :: Proxy ToPairs) $ _getState record

data IPythonWidget w =
       IPythonWidget
         { uuid :: UUID
         , state :: IORef (WidgetState w)
         }

-- | Change the value for a field, and notify the frontend about it. Doesn't work if the field is read only.
setField :: forall f w. (RElemOf f (WidgetFields w), ToKey f, IHaskellWidget (IPythonWidget w), ToPairs (Attr f))
         => IPythonWidget w -> FieldType f -> IO ()
setField widget fval = do
  attr <- getAttr @f widget
  when (_ro attr) $ error ("The field " ++ show (typeRep (Proxy @f)) ++ " is read only")
  !newattr <- setField' @f widget fval
  let pairs = toPairs newattr
  unless (null pairs) $ widgetSendUpdate widget (object pairs)

-- | Change the value of a field, without notifying the frontend and without checking if is read only. For internal use.
setField' :: forall f w. (RElemOf f (WidgetFields w), IHaskellWidget (IPythonWidget w), ToKey f)
          => IPythonWidget w -> FieldType f -> IO (Attr f)
setField' widget val = do
  attr <- getAttr @f widget
  newval <- _verify attr val
  let newattr = attr { _value = Real newval }
  modifyIORef (state widget) (\(WidgetState s) -> WidgetState (rput newattr s))
  return newattr

-- | Pluck an attribute from a record
getAttr :: forall f w. RElemOf f (WidgetFields w) => IPythonWidget w -> IO (Attr f)
#if MIN_VERSION_vinyl(0,9,0)
getAttr widget = rget <$> _getState <$> readIORef (state widget)
#else
getAttr widget = rget (Proxy @f) <$> _getState <$> readIORef (state widget)
#endif

-- | Get the value of a field.
getField :: forall f w. RElemOf f (WidgetFields w) => IPythonWidget w -> IO (FieldType f)
getField widget = unwrap . _value <$> getAttr @f widget

-- | Useful with toJSON and OverloadedStrings
str :: String -> String
str = id

properties :: IPythonWidget w -> IO ()
properties widget = do
  WidgetState st <- readIORef $ state widget
  let convert :: forall f. Attr f -> Const (TypeRep, TypeRep) f
      convert attr@(Attr {}) = Const (typeRep (Proxy @f), getFieldType attr)

      renderRow (fname, ftype) = printf "%s ::: %s" (show fname) (show ftype)
      rows = map renderRow . recordToList . rmap convert $ st
  mapM_ putStrLn rows
  
-- Helper function for widget to enforce their inability to fetch console input
noStdin :: IO a -> IO ()
noStdin action =
  let handler :: IOException -> IO ()
      handler e = when (ioeGetErrorType e == InvalidArgument)
                    (error "Widgets cannot do console input, sorry :)")
  in Ex.handle handler $ do
#if MIN_VERSION_unix(2,8,0)
    nullFd <- openFd "/dev/null" WriteOnly defaultFileFlags
#else
    nullFd <- openFd "/dev/null" WriteOnly Nothing defaultFileFlags
#endif
    oldStdin <- dup stdInput
    void $ dupTo nullFd stdInput
    closeFd nullFd
    void action
    void $ dupTo oldStdin stdInput

-- | Common function for the different trigger events
triggerEvent :: forall f w. (FieldType f ~ IO (), RElemOf f (WidgetFields w)) => IPythonWidget w -> IO ()
triggerEvent w = noStdin . join $ getField @f w

-- | Called when the value of an attribute is changed on the front-end
triggerChange :: RElemOf S.ChangeHandler (WidgetFields w) => IPythonWidget w -> IO ()
triggerChange = triggerEvent @ChangeHandler

-- | Called when the button is clicked
triggerClick :: RElemOf S.ClickHandler (WidgetFields w) => IPythonWidget w -> IO ()
triggerClick = triggerEvent @ClickHandler

-- | Called when a selection is made in a selection widget
triggerSelection :: RElemOf S.SelectionHandler (WidgetFields w) => IPythonWidget w -> IO ()
triggerSelection = triggerEvent @SelectionHandler

-- | Called when the text is submited in a text widget (or combobox/password)
triggerSubmit :: RElemOf S.SubmitHandler (WidgetFields w) => IPythonWidget w -> IO ()
triggerSubmit = triggerEvent @SubmitHandler

-- | Called when the widget is displayed on the notebook
triggerDisplay :: RElemOf S.DisplayHandler (WidgetFields w) => IPythonWidget w -> IO ()
triggerDisplay = triggerEvent @DisplayHandler

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

-- | Allows you to unlink a jslink
unlink :: (RElemOf S.Source (WidgetFields w), RElemOf S.Target (WidgetFields w), IHaskellWidget (IPythonWidget w))
       => IPythonWidget w
       -> IO (IPythonWidget w)
unlink w = do
  _ <- setField' @Source w EmptyWT
  _ <- setField' @Target w EmptyWT
  return w

data OutputMsg = OutputStream StreamType Text | OutputData Display deriving (Show)

instance ToJSON OutputMsg where
  toJSON (OutputStream n t) = object [ "output_type" .= str "stream"
                                     , "name" .= toJSON n
                                     , "text" .= toJSON t
                                     ]
  toJSON (OutputData d)     = object [ "output_type" .= str "display_data"
                                     , "data" .= toJSON d
                                     , "metadata" .= object []
                                     ]

