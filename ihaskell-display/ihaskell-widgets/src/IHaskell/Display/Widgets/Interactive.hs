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

module IHaskell.Display.Widgets.Interactive (interactive, usingHList, liftToWidgets) where

import           Data.Text
import           Data.Proxy

import           Data.Vinyl.Core
import           Data.Vinyl.Functor (Identity (..), Const (..))
import           Data.Vinyl.Derived (HList)
import           Data.Vinyl.Lens (type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           Data.Singletons.Prelude.List

import           IHaskell.Display

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import qualified IHaskell.Display.Widgets.Singletons as S (SField(..), Field(..))

import           IHaskell.Display.Widgets.Box.FlexBox
import           IHaskell.Display.Widgets.Bool.CheckBox
import           IHaskell.Display.Widgets.String.Text
import           IHaskell.Display.Widgets.Int.BoundedInt.IntSlider
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

-- | Convert a function to one accepting arguments in the form of an HList
usingHList :: WithTypes ts r -> HList ts -> r
usingHList f RNil = f
usingHList f (Identity x :& xs) = usingHList (f x) xs

-- Phantom type variables are required to make things play nicely with vinyl
data Constructor a where
  Constructor :: RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs
              => IO (IPythonWidget (SuitableWidget a)) -> Constructor a
newtype Getter a = Getter (IPythonWidget (SuitableWidget a) -> IO a)
newtype Setter a = Setter (IPythonWidget (SuitableWidget a) -> IO () -> IO ())
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

applySetters :: Rec Setter ts -> Rec RequiredWidget ts -> IO () -> IO ()
applySetters RNil RNil _ = return ()
applySetters (Setter setter :& xs) (RequiredWidget widget :& ws) handler = do
  setter widget handler
  applySetters xs ws handler

extractConstructor :: WidgetConf x -> Constructor x
extractConstructor (WidgetConf wr) = Constructor $ construct wr

extractGetter :: WidgetConf x -> Getter x
extractGetter (WidgetConf wr) = Getter $ getValue wr

extractSetter :: WidgetConf x -> Setter x
extractSetter (WidgetConf wr) = Setter $ setEvent wr

extractTrigger :: WidgetConf x -> Trigger x
extractTrigger (WidgetConf wr) = Trigger $ trigger wr

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

-- interactive :: (RecAll Identity ts FromWidget, IHaskellDisplay r, MakeConfs ts)
--             => WithTypes ts r -> IO FlexBox
-- interactive = undefined

-- | A version of interactive that workss with a function on HList instead of values
interactive :: (RecAll Identity ts FromWidget, IHaskellDisplay r, MakeConfs ts)
            => (HList ts -> r) -> IO FlexBox
interactive func = let confs = mkConfs Proxy
                   in liftToWidgets func confs

-- | Lift a function (HList ts -> r) to one using widgets to fill the HList and displaying the
-- output through the resultant widget.
liftToWidgets :: (RecAll Identity ts FromWidget, IHaskellDisplay r)
              => (HList ts -> r) -> Rec WidgetConf ts -> IO FlexBox
liftToWidgets func rc = do
  let constructors = rmap extractConstructor rc
      getters = rmap extractGetter rc
      setters = rmap extractSetter rc
      triggers = rmap extractTrigger rc

  bx <- mkFlexBox
  out <- mkOutputWidget

  -- Create a list of widgets
  widgets <- rtraverse createWidget constructors

  let handler = do
        vals <- applyGetters getters widgets
        replaceOutput out $ func vals

  -- Apply handler to all widgets
  applySetters setters widgets handler

  setField out Width 500
  setField bx Orientation VerticalOrientation

  -- Set children for the FlexBox
  let children = mkChildren widgets
  setField bx Children $ children ++ [ChildWidget out]

  return bx

data WrappedWidget w h f a where
  WrappedWidget :: (FieldType h ~ IO (), FieldType f ~ a, h ∈ WidgetFields w, f ∈ WidgetFields w,
                    ToPairs (Attr h), IHaskellWidget (IPythonWidget w))
                => IO (IPythonWidget w) -> S.SField h -> S.SField f -> WrappedWidget w h f a

construct :: WrappedWidget w h f a -> IO (IPythonWidget w)
construct (WrappedWidget cons _ _) = cons

getValue :: WrappedWidget w h f a -> IPythonWidget w -> IO a
getValue (WrappedWidget _ _ field) widget = getField widget field

setEvent :: WrappedWidget w h f a -> IPythonWidget w -> IO () -> IO ()
setEvent (WrappedWidget _ h _) = flip setField h

trigger :: WrappedWidget w h f a -> IPythonWidget w -> IO ()
trigger (WrappedWidget _ h _) = triggerEvent h

class RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs => FromWidget a where
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

instance FromWidget a => FromWidget (Identity a) where
  type SuitableWidget (Identity a) = SuitableWidget a
  type SuitableHandler (Identity a) = SuitableHandler a
  type SuitableField (Identity a) = SuitableField a
  wrapped = wrapped

-- interactive :: (FromWidget a, IHaskellDisplay b) => (a -> b) -> IO FlexBox
-- interactive func = do
--   let wrap = wrapped
--   widget <- construct wrap
--   bx <- mkFlexBox
--   out <- mkOutputWidget
--   setEvent wrap widget $ getValue wrap widget >>= replaceOutput out . func
--   trigger wrap widget
--   setField out Width 500
--   setField bx Orientation VerticalOrientation
--   setField bx Children [ChildWidget widget, ChildWidget out]
--   return bx
