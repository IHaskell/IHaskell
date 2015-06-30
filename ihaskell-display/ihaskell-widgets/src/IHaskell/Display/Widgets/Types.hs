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

import GHC.Exts (Constraint)
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Data.Dynamic
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Singletons.TH
import Data.Singletons.Prelude
import Data.Proxy
import qualified Data.Map as M
import IHaskell.Eval.Widgets
import IHaskell.Display (IHaskellWidget(..), Base64)
import IHaskell.IPython.Message.UUID

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
             | Disabled
             | StringValue
             | Placeholder
             | Tooltip
             | Icon
             | ButtonStyle
             | B64Value
             | ImageFormat
             deriving (Eq, Ord, Show)
             |]

instance ToJSON Field where
  toJSON ModelModule = "_model_module"
  toJSON ModelName = "_model_name"
  toJSON ViewModule = "_view_module"
  toJSON ViewName = "_view_name"
  toJSON MsgThrottle = "msg_throttle"
  toJSON Version = "version"
  toJSON Visible = "visible"
  toJSON CSS = "_css"
  toJSON DOMClasses = "_dom_classes"
  toJSON Width = "width"
  toJSON Height = "height"
  toJSON Padding = "padding"
  toJSON Margin = "margin"
  toJSON Color = "color"
  toJSON BackgroundColor = "background_color"
  toJSON BorderColor = "border_color"
  toJSON BorderWidth = "border_width"
  toJSON BorderRadius = "border_radius"
  toJSON BorderStyle = "border_style"
  toJSON FontStyle = "font_style"
  toJSON FontWeight = "font_weight"
  toJSON FontSize = "font_size"
  toJSON FontFamily = "font_family"
  toJSON Description = "description"
  toJSON ClickHandler = ""
  toJSON Disabled = "disabled"
  toJSON StringValue = "value"
  toJSON Placeholder = "placeholder"
  toJSON Tooltip = "tooltip"
  toJSON Icon = "icon"
  toJSON ButtonStyle = "button_style"
  toJSON B64Value = "_b64value"
  toJSON ImageFormat = "format"

-- | Reflect a (Proxy :: Proxy f) back to f
-- Copied from: http://stackoverflow.com/a/28033250/2388535
reflect ::
  forall (a :: k).
  (SingI a, SingKind ('KProxy :: KProxy k)) =>
  Proxy a -> Demote a
reflect _ = fromSing (sing :: Sing a)

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

type family FieldType (f :: Field) :: * where
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

  -- Multiple widgets/classes
  FieldType Description = Text
  FieldType ClickHandler = IO ()
  FieldType Disabled = Bool
  FieldType Tooltip = Text
  FieldType Icon = Text
  FieldType ButtonStyle = ButtonStyleValue
  FieldType B64Value = Base64
  FieldType ImageFormat = ImageFormatValue

class ToPairs a where
  toPairs :: a -> [Pair]

instance (SingI f, ToJSON (FieldType f)) => ToPairs (SField f, FieldType f) where
  toPairs (sfield :: SField f, ftype) = let field = reflect (Proxy :: Proxy f)
                                        in [field .= ftype]

-- Classes from IPython's widget hierarchy
type WidgetClass = '[ModelModule, ModelName, ViewModule, ViewName, MsgThrottle, Version, OnDisplayed]
type DOMWidgetClass = WidgetClass :++
    '[ Visible, CSS, DOMClasses, Width, Height, Padding, Margin, Color
     , BackgroundColor, BorderColor, BorderWidth, BorderStyle, FontStyle
     , FontWeight, FontSize, FontFamily
     ]
type StringClass = WidgetClass :++ '[StringValue, Disabled, Description, Placeholder]

data WidgetType = ButtonType
                | ImageType
                | OutputType
                | HTMLType
                | LatexType
                | TextType
                | TextAreaType

newtype WidgetState = WidgetState (IORef (M.Map Field Dynamic))

data Widget (w :: WidgetType) = Widget { uuid :: UUID, state :: WidgetState }

type family WidgetFields (w :: WidgetType) :: [Field] where
  WidgetFields ButtonType = DOMWidgetClass :++ '[Description, Tooltip, Disabled, Icon, ButtonStyle, ClickHandler]
  WidgetFields ImageType = DOMWidgetClass :++ '[ImageFormat, B64Value]
  WidgetFields OutputType = DOMWidgetClass
  WidgetFields HTMLType = StringClass
  WidgetFields LatexType = StringClass
  WidgetFields TextType = StringClass
  WidgetFields TextAreaType = StringClass

type family IsElem (x :: a) (xs :: [a]) :: Constraint where
  IsElem x xs = (IsElement x xs ~ True)

type family IsElement (x :: a) (xs :: [a]) :: Bool where
  IsElement y '[] = False
  IsElement y (y ': ys) = True
  IsElement y (x ': ys) = IsElement y ys

setField :: (Typeable (FieldType f), SingI f, f `IsElem` WidgetFields w, ToJSON (SField f, FieldType f), IHaskellWidget w) => Widget w -> SField f -> FieldType f -> IO ()
setField widget (sing :: SField f) val = do
  let field = reflect (Proxy :: Proxy f)
      WidgetState attrs = state widget
  modifyIORef attrs (M.insert field $ toDyn val)
  widgetSendUpdate widget $ toJSON (field, val)

getField :: (Typeable (FieldType f), SingI f, f `IsElem` WidgetFields w) => Widget w -> SField f -> IO (FieldType f)
getField widget (sing :: SField f) = do
  let field = reflect (Proxy :: Proxy f)
      WidgetState attrs = state widget
  valMap <- readIORef attrs
  case M.lookup field valMap of
    Nothing -> error "Couldn't find the field in IHaskell.Display.Widgets.Types.getField"
    Just x -> case fromDynamic x of
      Nothing -> error "Error casting types in IHaskell.Display.Widgets.Types.getField"
      Just y -> return y

-- | Enforce type-checked creation of attributes. Works well with Data.Map.fromList
(~=) :: (Typeable (FieldType f), SingI f) => SField f -> FieldType f -> (Field, Dynamic)
(sfield :: SField f) ~= ftype = (reflect (Proxy :: Proxy f), toDyn ftype)

-- | Default attributes for a DOMWidget
domWidgetWith :: FieldType ViewName -> [(Field, Dynamic)]
domWidgetWith viewName =
  [ SModelModule ~= ""
  , SModelName ~= "WidgetModel"
  , SViewModule ~= ""
  , SViewName ~= viewName
  , SMsgThrottle ~= 3
  , SVersion ~= 0
  , SOnDisplayed ~= return ()
  , SVisible ~= True
  , SCSS ~= []
  , SDOMClasses ~= []
  , SWidth ~= 0
  , SHeight ~= 0
  , SPadding ~= 0
  , SMargin ~= 0
  , SColor ~= ""
  , SBackgroundColor ~= ""
  , SBorderColor ~= ""
  , SBorderWidth ~= 0
  , SBorderRadius ~= 0
  , SBorderStyle ~= DefaultBorder
  , SFontStyle ~= DefaultFont
  , SFontWeight ~= DefaultWeight
  , SFontSize ~= 0
  , SFontFamily ~= ""
  ]

-- | Useful with toJSON
str :: String -> String
str = id
