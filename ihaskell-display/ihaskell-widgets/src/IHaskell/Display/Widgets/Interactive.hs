{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}

module IHaskell.Display.Widgets.Interactive (
  interactive,
  ArgList,
  (.&),
  pattern ArgNil,
  uncurryArgList,
  ) where

import           Data.Text
import           Data.Proxy

import           Data.Vinyl.Core
import           Data.Vinyl.Functor (Identity (..), Const (..))
import           Data.Vinyl.Derived (HList)
import           Data.Vinyl.Lens (type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           IHaskell.Display

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import qualified IHaskell.Display.Widgets.Singletons as S (SField(..), Field(..))

import           IHaskell.Display.Widgets.Box.FlexBox
import           IHaskell.Display.Widgets.Bool.CheckBox
import           IHaskell.Display.Widgets.String.Text
import           IHaskell.Display.Widgets.Int.BoundedInt.IntSlider
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatSlider
import           IHaskell.Display.Widgets.Output

data WidgetConf a where
  WidgetConf :: RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs
             => WrappedWidget (SuitableWidget a) (SuitableHandler a) (SuitableField a) a
             -> WidgetConf a

newtype WrappedConstructor a = WrappedConstructor {
  wrappedConstructor :: IO (IPythonWidget (SuitableWidget a))
}

type family WithTypes (ts :: [*]) (r :: *) :: * where
  WithTypes '[] r = r
  WithTypes (x ': xs) r = (x -> WithTypes xs r)

-- | Abstract heterogeneous list of arguments
newtype ArgList ts = ArgList { getList :: HList ts }

-- | Providing syntax to prevent having to use vinyl record style (@Identity x :& xs@) everywhere
infixr 9 .&
(.&) :: t -> ArgList ts -> ArgList (t ': ts)
x .& ArgList y = ArgList (Identity x :& y)

-- | Alias for empty arglist
pattern ArgNil = ArgList RNil

-- | Convert a function to one accepting an ArgList
uncurryArgList :: WithTypes ts r -> ArgList ts -> r
uncurryArgList f (ArgList RNil) = f
uncurryArgList f (ArgList (Identity x :& xs)) = uncurryArgList (f x) (ArgList xs)

-- Consistent type variables are required to make things play nicely with vinyl
data Constructor a where
  Constructor :: RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs
              => IO (IPythonWidget (SuitableWidget a)) -> Constructor a
newtype Getter a = Getter (IPythonWidget (SuitableWidget a) -> IO a)
newtype EventSetter a = EventSetter (IPythonWidget (SuitableWidget a) -> IO () -> IO ())
newtype ValueSetter a = ValueSetter (IPythonWidget (SuitableWidget a) -> a -> IO ())
newtype Trigger a = Trigger (IPythonWidget (SuitableWidget a) -> IO ())
data RequiredWidget a where
  RequiredWidget :: RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs
                 => IPythonWidget (SuitableWidget a)
                 -> RequiredWidget a

-- Zipping vinyl records in various ways
applyGetters :: Rec Getter ts -> Rec RequiredWidget ts -> IO (HList ts)
applyGetters RNil RNil = return RNil
applyGetters (Getter getter :& gs) (RequiredWidget widget :& ws) = do
  val <- getter widget
  rest <- applyGetters gs ws
  return $ Identity val :& rest

applyEventSetters :: Rec EventSetter ts -> Rec RequiredWidget ts -> IO () -> IO ()
applyEventSetters RNil RNil _ = return ()
applyEventSetters (EventSetter setter :& xs) (RequiredWidget widget :& ws) handler = do
  setter widget handler
  applyEventSetters xs ws handler

applyValueSetters :: Rec ValueSetter ts -> Rec RequiredWidget ts -> HList ts -> IO ()
applyValueSetters RNil RNil RNil = return ()
applyValueSetters (ValueSetter setter :& xs) (RequiredWidget widget :& ws) (Identity value :& vs) = do
  setter widget value
  applyValueSetters xs ws vs

extractConstructor :: WidgetConf x -> Constructor x
extractConstructor (WidgetConf wr) = Constructor $ construct wr

extractGetter :: WidgetConf x -> Getter x
extractGetter (WidgetConf wr) = Getter $ getValue wr

extractEventSetter :: WidgetConf x -> EventSetter x
extractEventSetter (WidgetConf wr) = EventSetter $ setEvent wr

extractTrigger :: WidgetConf x -> Trigger x
extractTrigger (WidgetConf wr) = Trigger $ trigger wr

extractValueSetter :: WidgetConf x -> ValueSetter x
extractValueSetter (WidgetConf wr) = ValueSetter $ setValue wr

createWidget :: Constructor a
             -> IO (RequiredWidget a)
createWidget (Constructor con) = fmap RequiredWidget con

mkChildren :: Rec RequiredWidget a -> [ChildWidget]
mkChildren widgets = let childRecord = rmap (\(RequiredWidget w) -> Const (ChildWidget w)) widgets
                     in recordToList childRecord

class MakeConfs (ts :: [*]) where
  mkConfs :: proxy ts -> Rec WidgetConf ts

instance MakeConfs '[] where
  mkConfs _ = RNil

instance (FromWidget t, MakeConfs ts) => MakeConfs (t ': ts) where
  mkConfs _ = WidgetConf wrapped :& mkConfs (Proxy :: Proxy ts)

-- | Interacting with a function on ArgList instead of values
interactive :: (IHaskellDisplay r, MakeConfs ts)
            => (ArgList ts -> r) -> ArgList ts -> IO FlexBox
interactive func = let confs = mkConfs Proxy
                   in liftToWidgets func confs

-- | Transform a function (ArgList ts -> r) to one using widgets to fill the ArgList, accepting
-- default values for those widgets and returning all widgets as a composite FlexBox widget with
-- and embedded OutputWidget for display.
liftToWidgets :: IHaskellDisplay r
              => (ArgList ts -> r) -> Rec WidgetConf ts -> ArgList ts -> IO FlexBox
liftToWidgets func rc defvals = do
  let constructors = rmap extractConstructor rc
      getters = rmap extractGetter rc
      eventSetters = rmap extractEventSetter rc
      valueSetters = rmap extractValueSetter rc
      triggers = rmap extractTrigger rc

  bx <- mkFlexBox
  out <- mkOutputWidget

  -- Create a list of widgets
  widgets <- rtraverse createWidget constructors

  let handler = do
        vals <- applyGetters getters widgets
        replaceOutput out $ func $ ArgList vals

  -- Apply handler to all widgets
  applyEventSetters eventSetters widgets handler

  -- Set default values for all widgets
  applyValueSetters valueSetters widgets $ getList defvals

  setField out Width 500
  setField bx Orientation VerticalOrientation

  -- Set children for the FlexBox
  let children = mkChildren widgets
  setField bx Children $ children ++ [ChildWidget out]

  return bx

data WrappedWidget w h f a where
  WrappedWidget :: (FieldType h ~ IO (), FieldType f ~ a, h ∈ WidgetFields w, f ∈ WidgetFields w,
                    ToPairs (Attr h), IHaskellWidget (IPythonWidget w), ToPairs (Attr f))
                => IO (IPythonWidget w) -> S.SField h -> S.SField f -> WrappedWidget w h f a

construct :: WrappedWidget w h f a -> IO (IPythonWidget w)
construct (WrappedWidget cons _ _) = cons

getValue :: WrappedWidget w h f a -> IPythonWidget w -> IO a
getValue (WrappedWidget _ _ field) widget = getField widget field

setValue :: WrappedWidget w h f a -> IPythonWidget w -> a -> IO ()
setValue (WrappedWidget _ _ field) widget = setField widget field

setEvent :: WrappedWidget w h f a -> IPythonWidget w -> IO () -> IO ()
setEvent (WrappedWidget _ h _) widget = setField widget h

trigger :: WrappedWidget w h f a -> IPythonWidget w -> IO ()
trigger (WrappedWidget _ h _) = triggerEvent h

class (RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs) => FromWidget a where
  type SuitableWidget a :: WidgetType
  type SuitableHandler a :: S.Field
  type SuitableField a :: S.Field
  wrapped :: WrappedWidget (SuitableWidget a) (SuitableHandler a) (SuitableField a) a

instance FromWidget Bool where
  type SuitableWidget Bool = CheckBoxType
  type SuitableHandler Bool = S.ChangeHandler
  type SuitableField Bool = S.BoolValue
  wrapped = WrappedWidget mkCheckBox ChangeHandler BoolValue

instance FromWidget Text where
  type SuitableWidget Text = TextType
  type SuitableHandler Text = S.SubmitHandler
  type SuitableField Text = S.StringValue
  wrapped = WrappedWidget mkTextWidget SubmitHandler StringValue

instance FromWidget Integer where
  type SuitableWidget Integer = IntSliderType
  type SuitableHandler Integer = S.ChangeHandler
  type SuitableField Integer = S.IntValue
  wrapped = WrappedWidget mkIntSlider ChangeHandler IntValue

instance FromWidget Double where
  type SuitableWidget Double = FloatSliderType
  type SuitableHandler Double = S.ChangeHandler
  type SuitableField Double = S.FloatValue
  wrapped = WrappedWidget mkFloatSlider ChangeHandler FloatValue
