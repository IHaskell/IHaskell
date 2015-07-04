{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
module IHaskell.Display.Widgets.Types where

import Control.Monad (when)

import Data.Aeson
import Data.Aeson.Types (emptyObject, Pair)
import Data.Text (pack, Text)
import Data.IORef (IORef, readIORef, modifyIORef)
import Data.Proxy

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

-- Classes from IPython's widget hierarchy
type WidgetClass = '[ModelModule, ModelName, ViewModule, ViewName, MsgThrottle, Version, OnDisplayed]
type DOMWidgetClass = WidgetClass :++
   '[ Visible, CSS, DOMClasses, Width, Height, Padding, Margin, Color
    , BackgroundColor, BorderColor, BorderWidth, BorderRadius, BorderStyle, FontStyle
    , FontWeight, FontSize, FontFamily
    ]
type StringClass = DOMWidgetClass :++ '[StringValue, Disabled, Description, Placeholder]
type BoolClass = DOMWidgetClass :++ '[BoolValue, Disabled, Description]

-- Types associated with Fields
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

data WidgetType = ButtonType
                | ImageType
                | OutputType
                | HTMLType
                | LatexType
                | TextType
                | TextAreaType
                | CheckBoxType
                | ToggleButtonType

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

newtype Attr f = Attr { _unAttr :: FieldType f }

class ToPairs a where
  toPairs :: a -> [Pair]

-- Attributes that aren't synced with the frontend give [] on toPairs
instance ToPairs (Attr ModelModule) where toPairs (Attr x) = ["_model_module" .= toJSON x]
instance ToPairs (Attr ModelName) where toPairs (Attr x) = ["_model_name" .= toJSON x]
instance ToPairs (Attr ViewModule) where toPairs (Attr x) = ["_view_module" .= toJSON x]
instance ToPairs (Attr ViewName) where toPairs (Attr x) = ["_view_name" .= toJSON x]
instance ToPairs (Attr MsgThrottle) where toPairs (Attr x) = ["msg_throttle" .= toJSON x]
instance ToPairs (Attr Version) where toPairs (Attr x) = ["version" .= toJSON x]
instance ToPairs (Attr OnDisplayed) where toPairs (Attr x) = [] -- Not sent to the frontend
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
instance ToPairs (Attr ClickHandler) where toPairs (Attr x) = [] -- Not sent to the frontend
instance ToPairs (Attr SubmitHandler) where toPairs (Attr x) = [] -- Not sent to the frontend
instance ToPairs (Attr Disabled) where toPairs (Attr x) = ["disabled" .= toJSON x]
instance ToPairs (Attr StringValue) where toPairs (Attr x) = ["value" .= toJSON x]
instance ToPairs (Attr Placeholder) where toPairs (Attr x) = ["placeholder" .= toJSON x]
instance ToPairs (Attr Tooltip) where toPairs (Attr x) = ["tooltip" .= toJSON x]
instance ToPairs (Attr Icon) where toPairs (Attr x) = ["icon" .= toJSON x]
instance ToPairs (Attr ButtonStyle) where toPairs (Attr x) = ["button_style" .= toJSON x]
instance ToPairs (Attr B64Value) where toPairs (Attr x) = ["_b64value" .= toJSON x]
instance ToPairs (Attr ImageFormat) where toPairs (Attr x) = ["format" .= toJSON x]
instance ToPairs (Attr BoolValue) where toPairs (Attr x) = ["value" .= toJSON x]

(=::) :: sing f -> FieldType f -> Attr f
_ =:: x = Attr x

defaultWidget :: FieldType ViewName -> Rec Attr WidgetClass
defaultWidget viewName = (SModelModule =:: "")
                      :& (SModelName =:: "WidgetModel")
                      :& (SViewModule =:: "")
                      :& (SViewName =:: viewName)
                      :& (SMsgThrottle =:: 3)
                      :& (SVersion =:: 0)
                      :& (SOnDisplayed =:: return ())
                      :& RNil

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

defaultStringWidget :: FieldType ViewName -> Rec Attr StringClass
defaultStringWidget viewName = defaultDOMWidget viewName <+> strAttrs
  where strAttrs = (SStringValue =:: "")
                :& (SDisabled =:: False)
                :& (SDescription =:: "")
                :& (SPlaceholder =:: "")
                :& RNil

defaultBoolWidget :: FieldType ViewName -> Rec Attr BoolClass
defaultBoolWidget viewName = defaultDOMWidget viewName <+> boolAttrs
  where boolAttrs = (SBoolValue =:: False)
                 :& (SDisabled =:: False)
                 :& (SDescription =:: "")
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

data Widget (w :: WidgetType) = Widget { uuid :: UUID, state :: IORef (WidgetState w) }

-- | Reflect a (Proxy :: Proxy f) back to f
-- Copied from: http://stackoverflow.com/a/28033250/2388535
reflect ::
  forall (a :: k).
  (SingI a, SingKind ('KProxy :: KProxy k)) =>
  Proxy a -> Demote a
reflect _ = fromSing (sing :: Sing a)

-- | Change the value for a field, and notify the frontend about it.
setField :: (f ∈ WidgetFields w, IHaskellWidget (Widget w), SingI f, ToPairs (Attr f)) => Widget w -> SField f -> FieldType f -> IO ()
setField widget (sfield :: SField f) fval = do
  setField' widget sfield fval
  let pairs = toPairs (Attr fval :: Attr f)
  when (not . null $ pairs) $ widgetSendUpdate widget (object pairs)

-- | Change the value of a field, without notifying the frontend. For internal use. Uses BangPattern.
setField' :: (f ∈ WidgetFields w, IHaskellWidget (Widget w), SingI f) => Widget w -> SField f -> FieldType f -> IO ()
setField' widget (sfield :: SField f) !fval = modifyIORef (state widget) (WidgetState . rput (sfield =:: fval) . _getState)

-- | Get the value of a field.
getField :: (f ∈ WidgetFields w) => Widget w -> SField f -> IO (FieldType f)
getField widget sfield = _unAttr <$> rget sfield <$> _getState <$> readIORef (state widget)

-- | Useful with toJSON, and OverloadedStrings
str :: String -> String
str = id

instance ToJSON Natural where
  toJSON 0 = String ""
  toJSON n = String . pack $ show n
