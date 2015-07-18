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
-- The IPython widgets expect state updates of the form {"property": value}, where an empty string for
-- numeric values is ignored by the frontend and the default value is used instead.
--
-- To know more about the IPython messaging specification (as implemented in this package) take a look
-- at the supplied MsgSpec.md.

import Control.Monad (when)
import Control.Applicative ((<$>))

import Data.Aeson
import Data.Aeson.Types (emptyObject, Pair)
import Data.Text (pack, Text)
import Data.IORef (IORef, readIORef, modifyIORef)

import Data.Vinyl (Rec (..), (<+>), recordToList, reifyConstraint, rmap, Dict (..))
import Data.Vinyl.Functor (Compose (..), Const (..))
import Data.Vinyl.Lens (rget, rput, type (∈))
import Data.Vinyl.TypeLevel (RecAll (..))

import Data.Singletons.Prelude ((:++))
import Data.Singletons.TH

import Numeric.Natural

import IHaskell.Eval.Widgets (widgetSendUpdate)
import IHaskell.Display (Base64, IHaskellWidget (..))
import IHaskell.IPython.Message.UUID

import IHaskell.Display.Widgets.Common

-- Classes from IPython's widget hierarchy. Defined as such to reduce code duplication.
type WidgetClass = '[ModelModule, ModelName, ViewModule, ViewName, MsgThrottle, Version, OnDisplayed]
type DOMWidgetClass = WidgetClass :++
   '[ Visible, CSS, DOMClasses, Width, Height, Padding, Margin, Color
    , BackgroundColor, BorderColor, BorderWidth, BorderRadius, BorderStyle, FontStyle
    , FontWeight, FontSize, FontFamily
    ]
type StringClass = DOMWidgetClass :++ '[StringValue, Disabled, Description, Placeholder]
type BoolClass = DOMWidgetClass :++ '[BoolValue, Disabled, Description]
type SelectionClass = DOMWidgetClass :++
   '[Options, SelectedValue, SelectedLabel, Disabled, Description, SelectionHandler]
type MultipleSelectionClass = DOMWidgetClass :++
   '[Options, SelectedLabels, SelectedValues, Disabled, Description, SelectionHandler]

-- Types associated with Fields.
type family FieldType (f :: Field) :: * where
  FieldType ModelModule = Text
  FieldType ModelName = Text
  FieldType ViewModule = Text
  FieldType ViewName = Text
  FieldType MsgThrottle = Natural
  FieldType Version = Natural
  FieldType OnDisplayed = IO ()
  FieldType Visible = Bool
  FieldType CSS = [(Text, Text, Text)]
  FieldType DOMClasses = [Text]
  FieldType Width = Natural
  FieldType Height = Natural
  FieldType Padding = Natural
  FieldType Margin = Natural
  FieldType Color = Text
  FieldType BackgroundColor = Text
  FieldType BorderColor = Text
  FieldType BorderWidth = Natural
  FieldType BorderRadius = Natural
  FieldType BorderStyle = BorderStyleValue
  FieldType FontStyle = FontStyleValue
  FieldType FontWeight = FontWeightValue
  FieldType FontSize = Natural
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

-- Fields associated with a widget
type family WidgetFields (w :: WidgetType) :: [Field] where
  WidgetFields ButtonType = DOMWidgetClass :++ '[Description, Tooltip, Disabled, Icon, ButtonStyle, ClickHandler]
  WidgetFields ImageType = DOMWidgetClass :++ '[ImageFormat, B64Value]
  WidgetFields OutputType = DOMWidgetClass
  WidgetFields HTMLType = StringClass
  WidgetFields LatexType = StringClass
  WidgetFields TextType = StringClass :++ '[SubmitHandler]
  WidgetFields TextAreaType = StringClass
  WidgetFields CheckBoxType = BoolClass
  WidgetFields ToggleButtonType = BoolClass :++ '[Tooltip, Icon, ButtonStyle]
  WidgetFields DropdownType = SelectionClass :++ '[ButtonStyle]
  WidgetFields RadioButtonsType = SelectionClass
  WidgetFields SelectType = SelectionClass
  WidgetFields ToggleButtonsType = SelectionClass :++ '[Tooltips, Icons, ButtonStyle]
  WidgetFields SelectMultipleType = MultipleSelectionClass

-- Wrapper around a field
newtype Attr (f :: Field) = Attr { _unAttr :: FieldType f }

-- Types that can be converted to Aeson Pairs.
class ToPairs a where
  toPairs :: a -> [Pair]

-- Attributes that aren't synced with the frontend give [] on toPairs
instance ToPairs (Attr ModelModule) where toPairs (Attr x) = ["_model_module" .= toJSON x]
instance ToPairs (Attr ModelName) where toPairs (Attr x) = ["_model_name" .= toJSON x]
instance ToPairs (Attr ViewModule) where toPairs (Attr x) = ["_view_module" .= toJSON x]
instance ToPairs (Attr ViewName) where toPairs (Attr x) = ["_view_name" .= toJSON x]
instance ToPairs (Attr MsgThrottle) where toPairs (Attr x) = ["msg_throttle" .= toJSON x]
instance ToPairs (Attr Version) where toPairs (Attr x) = ["version" .= toJSON x]
instance ToPairs (Attr OnDisplayed) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Visible) where toPairs (Attr x) = ["visible" .= toJSON x]
instance ToPairs (Attr CSS) where toPairs (Attr x) = ["_css" .= toJSON x]
instance ToPairs (Attr DOMClasses) where toPairs (Attr x) = ["_dom_classes" .= toJSON x]
instance ToPairs (Attr Width) where toPairs (Attr x) = ["width" .= toJSON x]
instance ToPairs (Attr Height) where toPairs (Attr x) = ["height" .= toJSON x]
instance ToPairs (Attr Padding) where toPairs (Attr x) = ["padding" .= toJSON x]
instance ToPairs (Attr Margin) where toPairs (Attr x) = ["margin" .= toJSON x]
instance ToPairs (Attr Color) where toPairs (Attr x) = ["color" .= toJSON x]
instance ToPairs (Attr BackgroundColor) where toPairs (Attr x) = ["background_color" .= toJSON x]
instance ToPairs (Attr BorderColor) where toPairs (Attr x) = ["border_color" .= toJSON x]
instance ToPairs (Attr BorderWidth) where toPairs (Attr x) = ["border_width" .= toJSON x]
instance ToPairs (Attr BorderRadius) where toPairs (Attr x) = ["border_radius" .= toJSON x]
instance ToPairs (Attr BorderStyle) where toPairs (Attr x) = ["border_style" .= toJSON x]
instance ToPairs (Attr FontStyle) where toPairs (Attr x) = ["font_style" .= toJSON x]
instance ToPairs (Attr FontWeight) where toPairs (Attr x) = ["font_weight" .= toJSON x]
instance ToPairs (Attr FontSize) where toPairs (Attr x) = ["font_size" .= toJSON x]
instance ToPairs (Attr FontFamily) where toPairs (Attr x) = ["font_family" .= toJSON x]
instance ToPairs (Attr Description) where toPairs (Attr x) = ["description" .= toJSON x]
instance ToPairs (Attr ClickHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr SubmitHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Disabled) where toPairs (Attr x) = ["disabled" .= toJSON x]
instance ToPairs (Attr StringValue) where toPairs (Attr x) = ["value" .= toJSON x]
instance ToPairs (Attr Placeholder) where toPairs (Attr x) = ["placeholder" .= toJSON x]
instance ToPairs (Attr Tooltip) where toPairs (Attr x) = ["tooltip" .= toJSON x]
instance ToPairs (Attr Icon) where toPairs (Attr x) = ["icon" .= toJSON x]
instance ToPairs (Attr ButtonStyle) where toPairs (Attr x) = ["button_style" .= toJSON x]
instance ToPairs (Attr B64Value) where toPairs (Attr x) = ["_b64value" .= toJSON x]
instance ToPairs (Attr ImageFormat) where toPairs (Attr x) = ["format" .= toJSON x]
instance ToPairs (Attr BoolValue) where toPairs (Attr x) = ["value" .= toJSON x]
instance ToPairs (Attr SelectedLabel) where toPairs (Attr x) = ["selected_label" .= toJSON x]
instance ToPairs (Attr SelectedValue) where toPairs (Attr x) = ["value" .= toJSON x]
instance ToPairs (Attr Options) where
  toPairs (Attr x) = case x of
    OptionLabels xs -> labels xs
    OptionDict xps -> labels $ map fst xps
    where labels xs = ["_options_labels" .= xs]
instance ToPairs (Attr SelectionHandler) where toPairs _ = [] -- Not sent to the frontend
instance ToPairs (Attr Tooltips) where toPairs (Attr x) = ["tooltips" .= toJSON x]
instance ToPairs (Attr Icons) where toPairs (Attr x) = ["icons" .= toJSON x]
instance ToPairs (Attr SelectedLabels) where toPairs (Attr x) = ["selected_labels" .= toJSON x]
instance ToPairs (Attr SelectedValues) where toPairs (Attr x) = ["values" .= toJSON x]

-- | Store the value for a field, as an object parametrized by the Field
(=::) :: sing f -> FieldType f -> Attr f
_ =:: x = Attr x

-- | A record representing an object of the Widget class from IPython
defaultWidget :: FieldType ViewName -> Rec Attr WidgetClass
defaultWidget viewName = (SModelModule =:: "")
                      :& (SModelName =:: "WidgetModel")
                      :& (SViewModule =:: "")
                      :& (SViewName =:: viewName)
                      :& (SMsgThrottle =:: 3)
                      :& (SVersion =:: 0)
                      :& (SOnDisplayed =:: return ())
                      :& RNil

-- | A record representing an object of the DOMWidget class from IPython
defaultDOMWidget :: FieldType ViewName -> Rec Attr DOMWidgetClass
defaultDOMWidget viewName = defaultWidget viewName <+> domAttrs
  where domAttrs = (SVisible =:: True)
                :& (SCSS =:: [])
                :& (SDOMClasses =:: [])
                :& (SWidth =:: 0)
                :& (SHeight =:: 0)
                :& (SPadding =:: 0)
                :& (SMargin =:: 0)
                :& (SColor =:: "")
                :& (SBackgroundColor =:: "")
                :& (SBorderColor =:: "")
                :& (SBorderWidth =:: 0)
                :& (SBorderRadius =:: 0)
                :& (SBorderStyle =:: DefaultBorder)
                :& (SFontStyle =:: DefaultFont)
                :& (SFontWeight =:: DefaultWeight)
                :& (SFontSize =:: 0)
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
setField :: (f ∈ WidgetFields w, IHaskellWidget (IPythonWidget w), ToPairs (Attr f)) => IPythonWidget w -> SField f -> FieldType f -> IO ()
setField widget (sfield :: SField f) fval = do
  setField' widget sfield fval
  let pairs = toPairs (Attr fval :: Attr f)
  when (not . null $ pairs) $ widgetSendUpdate widget (object pairs)

-- | Change the value of a field, without notifying the frontend. For internal use. Uses BangPattern.
setField' :: (f ∈ WidgetFields w, IHaskellWidget (IPythonWidget w)) => IPythonWidget w -> SField f -> FieldType f -> IO ()
setField' widget sfield !fval = modifyIORef (state widget) (WidgetState . rput (sfield =:: fval) . _getState)

-- | Get the value of a field.
getField :: (f ∈ WidgetFields w) => IPythonWidget w -> SField f -> IO (FieldType f)
getField widget sfield = _unAttr <$> rget sfield <$> _getState <$> readIORef (state widget)

-- | Useful with toJSON and OverloadedStrings
str :: String -> String
str = id

-- | Send zero values as empty strings, which stands for default value in the frontend.
instance ToJSON Natural where
  toJSON 0 = String ""
  toJSON n = String . pack $ show n


