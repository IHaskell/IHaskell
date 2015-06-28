{-# LANGUAGE FlexibleContexts #-}
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
{-# LANGUAGE AutoDeriveTypeable #-}

module IHaskell.Display.Widgets.Types where

import Data.Text
import Data.Dynamic
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Singletons.TH
import Data.Proxy
import qualified Data.Map as M
import IHaskell.IPython.Message.UUID

singletons [d|
  data Field = CommUUID
             | ModelModule
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
             | Disabled
             | Placeholder
             | Tooltip
             | Icon
             | ButtonStyle
             | B64Value
             | Icons
             | ImageFormat
             | StringValue
             | Tooltips
             deriving (Eq, Ord, Show)
             |]

-- | Reflect a (Proxy :: Proxy f) back to f
-- Copied from: http://stackoverflow.com/a/28033250/2388535
reflect ::
  forall (a :: k).
  (SingI a, SingKind ('KProxy :: KProxy k)) =>
  Proxy a -> Demote a
reflect _ = fromSing (sing :: Sing a)

data BorderStyleValue = NoBorder -- 'none'
                      | HiddenBorder -- 'hidden'
                      | DottedBorder -- 'dotted'
                      | DashedBorder -- 'dashed'
                      | SolidBorder -- 'solid'
                      | DoubleBorder -- 'double'
                      | GrooveBorder -- 'groove'
                      | RidgeBorder -- 'ridge'
                      | InsetBorder -- 'inset'
                      | OutsetBorder -- 'outset'
                      | InitialBorder -- 'initial'
                      | InheritBorder -- 'inherit'
                      | DefaultBorder -- ''

data FontStyleValue = NormalFont -- 'normal'
                    | ItalicFont -- 'italic'
                    | ObliqueFont -- 'oblique'
                    | InitialFont -- 'initial'
                    | InheritFont -- 'inherit'
                    | DefaultFont -- ''

data FontWeightValue = NormalWeight -- 'normal'
                     | BoldWeight -- 'bold'
                     | BolderWeight -- 'bolder'
                     | LighterWeight -- 'lighter'
                     | InheritWeight -- 'inherit'
                     | InitialWeight -- 'initial'
                     | DefaultWeight -- ''

type family FieldType (f :: Field) :: * where
  FieldType CommUUID = UUID

  -- WidgetClass
  FieldType ModelModule = Text
  FieldType ModelName = Text
  FieldType ViewModule = Text
  FieldType ViewName = Text
  FieldType MsgThrottle = Int
  FieldType Version = Int
  FieldType OnDisplayed = IO ()

  -- DOMWidgetClass
  FieldType Visible = Bool
  FieldType CSS = [(Text, Text, Text)]
  FieldType DOMClasses = [Text]
  FieldType Width = Int
  FieldType Height = Int
  FieldType Padding = Int
  FieldType Margin = Int
  FieldType Color = Text
  FieldType BackgroundColor = Text
  FieldType BorderColor = Text
  FieldType BorderWidth = Int
  FieldType BorderRadius = Int
  FieldType BorderStyle = BorderStyleValue
  FieldType FontStyle = FontStyleValue
  FieldType FontWeight = FontWeightValue
  FieldType FontSize = Int
  FieldType FontFamily = Text

  -- StringClass
  FieldType StringValue = Text
  FieldType Placeholder = Text

  -- Other widgets
  FieldType Description = Text
  FieldType ClickHandler = Text -> IO ()
  FieldType Disabled = Bool

type family Append (xs :: [k]) (ys :: [k]) :: [k] where
  Append '[] xs = xs
  Append xs '[] = xs
  Append (x ': xs) ys = x ': Append xs ys

type WidgetClass = '[ModelModule, ModelName, ViewModule, ViewName, MsgThrottle, Version, OnDisplayed]
type DOMWidgetClass = Append WidgetClass
    '[ Visible, CSS, DOMClasses, Width, Height, Padding, Margin, Color
     , BackgroundColor, BorderColor, BorderWidth, BorderStyle, FontStyle
     , FontWeight, FontSize, FontFamily
     ]
type StringClass = Append WidgetClass '[StringValue, Disabled, Description, Placeholder]

data WidgetType = ButtonType
                | ImageType
                | OutputType
                | HTMLType
                | LatexType
                | TextType
                | TextAreaType

data Widget (w :: WidgetType) = Widget { attrs :: IORef (M.Map Field Dynamic) }

type family WidgetFields (w :: WidgetType) :: [Field] where
  WidgetFields ButtonType = Append DOMWidgetClass '[Description, Tooltip, Disabled, Icon, ButtonStyle, ClickHandler]
  WidgetFields ImageType = Append DOMWidgetClass '[ImageFormat, B64Value]
  WidgetFields OutputType = DOMWidgetClass
  WidgetFields HTMLType = StringClass
  WidgetFields LatexType = StringClass
  WidgetFields TextType = StringClass
  WidgetFields TextAreaType = StringClass

type family IsElem (xs :: [a]) (x :: a) :: Bool where
  IsElem '[] y = False
  IsElem (y ': ys) y = True
  IsElem (x ': ys) y = IsElem ys y

setField :: (Typeable (FieldType f), SingI f, IsElem (WidgetFields w) f ~ True) => Widget w -> Proxy f -> FieldType f -> IO ()
setField widget proxy val = do
  let field = reflect proxy
  modifyIORef (attrs widget) (M.insert field $ toDyn val)

getField :: (Typeable (FieldType f), SingI f, IsElem (WidgetFields w) f ~ True) => Widget w -> Proxy f -> IO (FieldType f)
getField widget proxy = do
  let field = reflect proxy
  valMap <- readIORef $ attrs widget
  case M.lookup field valMap of
    Nothing -> error "Couldn't find the field in IHaskell.Display.Widgets.Types.getField"
    Just x -> case fromDynamic x of
      Nothing -> error "Error casting types in IHaskell.Display.Widgets.Types.getField"
      Just y -> return y
