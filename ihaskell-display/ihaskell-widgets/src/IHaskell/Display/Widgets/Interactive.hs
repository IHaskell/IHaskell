{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module IHaskell.Display.Widgets.Interactive
  ( interactive
  , uncurryHList
  , Rec (..)
  , Argument(..)
  ) where

import           Data.Text
import           Data.Proxy

#if MIN_VERSION_vinyl(0,9,0)
import           Data.Vinyl.Core (Rec(..))
import           Data.Vinyl.Recursive (recordToList, rmap, rtraverse)
#else
import           Data.Vinyl.Core (Rec(..), recordToList, rmap, rtraverse)
#endif
import           Data.Vinyl.Functor (Identity(..), Const(..))
import           Data.Vinyl.Derived (HList)
import           Data.Vinyl.Lens (type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           IHaskell.Display

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import qualified IHaskell.Display.Widgets.Singletons as S

import           IHaskell.Display.Widgets.Box.Box
import           IHaskell.Display.Widgets.Bool.CheckBox
import           IHaskell.Display.Widgets.String.Text
import           IHaskell.Display.Widgets.Int.BoundedInt.IntSlider
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatSlider
import           IHaskell.Display.Widgets.Output


data WidgetConf a where
        WidgetConf ::
            (RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs,
             FromWidget a) =>
            WrappedWidget (SuitableWidget a) (SuitableHandler a)
              (SuitableField a)
              a
              -> WidgetConf a


type family WithTypes (ts :: [*]) (r :: *) :: * where
        WithTypes '[] r = r
        WithTypes (x ': xs) r = (x -> WithTypes xs r)

uncurryHList :: WithTypes ts r -> HList ts -> r
uncurryHList f RNil = f
uncurryHList f (Identity x :& xs) = uncurryHList (f x) xs

-- Consistent type variables are required to make things play nicely with vinyl

data Constructor a where
        Constructor ::
            RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs =>
            IO (IPythonWidget (SuitableWidget a)) -> Constructor a

newtype Getter a = Getter (IPythonWidget (SuitableWidget a) -> IO a)

newtype EventSetter a = EventSetter (IPythonWidget (SuitableWidget a) -> IO () -> IO ())

newtype Initializer a = Initializer (IPythonWidget (SuitableWidget a) -> Argument a -> IO ())


data RequiredWidget a where
        RequiredWidget ::
            RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs =>
            IPythonWidget (SuitableWidget a) -> RequiredWidget a

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

setInitialValues :: Rec Initializer ts -> Rec RequiredWidget ts -> Rec Argument ts -> IO ()
setInitialValues RNil RNil RNil = return ()
setInitialValues (Initializer initialize :& fs) (RequiredWidget widget :& ws) (argument :& vs) = do
  initialize widget argument
  setInitialValues fs ws vs

extractConstructor :: WidgetConf x -> Constructor x
extractConstructor (WidgetConf wr) = Constructor $ construct wr

extractGetter :: WidgetConf x -> Getter x
extractGetter (WidgetConf wr) = Getter $ getValue wr

extractEventSetter :: WidgetConf x -> EventSetter x
extractEventSetter (WidgetConf wr) = EventSetter $ setEvent wr

extractInitializer :: WidgetConf x -> Initializer x
extractInitializer WidgetConf{} = Initializer initializer

createWidget :: Constructor a -> IO (RequiredWidget a)
createWidget (Constructor con) = fmap RequiredWidget con

mkChildren :: Rec RequiredWidget a -> [ChildWidget]
mkChildren widgets =
  let childRecord = rmap (\(RequiredWidget w) -> Const (ChildWidget w)) widgets
  in recordToList childRecord

class MakeConfs (ts :: [*]) where
  mkConfs :: proxy ts -> Rec WidgetConf ts

instance MakeConfs '[] where
  mkConfs _ = RNil

instance (FromWidget t, MakeConfs ts) => MakeConfs (t ': ts) where
  mkConfs _ = WidgetConf wrapped :& mkConfs (Proxy :: Proxy ts)

interactive :: (IHaskellDisplay r, MakeConfs ts)
            => (HList ts -> r) -> Rec Argument ts -> IO Box
interactive func =
  let confs = mkConfs Proxy
  in liftToWidgets func confs

-- | Transform a function (HList ts -> r) to one which: 1) Uses widgets to accept the arguments 2)
-- Accepts initial values for the arguments 3) Creates a compound Box widget with an embedded
-- OutputWidget for display
liftToWidgets :: IHaskellDisplay r
              => (HList ts -> r) -> Rec WidgetConf ts -> Rec Argument ts -> IO Box
liftToWidgets func rc initvals = do
  let constructors = rmap extractConstructor rc
      getters = rmap extractGetter rc
      eventSetters = rmap extractEventSetter rc
      initializers = rmap extractInitializer rc

  bx <- mkBox
  out <- mkOutput

  -- Create a list of widgets
  widgets <- rtraverse createWidget constructors

  let handler = do
        vals <- applyGetters getters widgets
        replaceOutput out $ func vals

  -- Apply handler to all widgets
  applyEventSetters eventSetters widgets handler

  -- Set initial values for all widgets
  setInitialValues initializers widgets initvals
  -- applyValueSetters valueSetters widgets $ getList defvals
  -- setField out Width 500
  -- TODO This can't be set right now since we switched FlexBox to a regular
  --      Box. This is a styling/layout parameter now but these haven't been implemented yet.
  -- setField bx Orientation VerticalOrientation

  -- Set children for the Box
  let children = mkChildren widgets
  setField @Children bx $ children ++ [ChildWidget out]

  return bx


data WrappedWidget w h f a where
        WrappedWidget ::
            forall h f w a. (FieldType h ~ IO (), FieldType f ~ a, h ∈ WidgetFields w,
             f ∈ WidgetFields w, ToPairs (Attr h), ToKey h,
             IHaskellWidget (IPythonWidget w), ToPairs (Attr f)) =>
            IO (IPythonWidget w) -> WrappedWidget w h f a

construct :: WrappedWidget w h f a -> IO (IPythonWidget w)
construct (WrappedWidget cs) = cs

getValue :: forall w h f a. WrappedWidget w h f a -> IPythonWidget w -> IO a
getValue (WrappedWidget _) widget = getField @f widget

setEvent :: forall w h f a. WrappedWidget w h f a -> IPythonWidget w -> IO () -> IO ()
setEvent (WrappedWidget _) widget = setField @h widget

class RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs => FromWidget a where
  type SuitableWidget a
  type SuitableHandler a
  type SuitableField a
  data Argument a
  initializer :: IPythonWidget (SuitableWidget a) -> Argument a -> IO ()
  wrapped :: WrappedWidget (SuitableWidget a) (SuitableHandler a) (SuitableField a) a

instance FromWidget Bool where
  type SuitableWidget Bool = CheckBoxType
  type SuitableHandler Bool = S.ChangeHandler
  type SuitableField Bool = S.BoolValue
  data Argument Bool = BoolVal Bool
  initializer w (BoolVal b) = setField @BoolValue w b
  wrapped = WrappedWidget @ChangeHandler @BoolValue mkCheckBox

instance FromWidget Text where
  type SuitableWidget Text = TextType
  type SuitableHandler Text = S.SubmitHandler
  type SuitableField Text = S.StringValue
  data Argument Text = TextVal Text
  initializer w (TextVal txt) = setField @StringValue w txt
  wrapped = WrappedWidget @SubmitHandler @StringValue mkText

instance FromWidget Integer where
  type SuitableWidget Integer = IntSliderType
  type SuitableHandler Integer = S.ChangeHandler
  type SuitableField Integer = S.IntValue
  data Argument Integer = IntVal Integer
                      | IntRange (Integer, Integer, Integer)
  wrapped = WrappedWidget @ChangeHandler @IntValue mkIntSlider
  initializer w (IntVal int) = setField @IntValue w int
  initializer w (IntRange (v, l, u)) = do
    setField @IntValue w v
    setField @MinInt w l
    setField @MaxInt w u

instance FromWidget Double where
  type SuitableWidget Double = FloatSliderType
  type SuitableHandler Double = S.ChangeHandler
  type SuitableField Double = S.FloatValue
  data Argument Double = FloatVal Double
                     | FloatRange (Double, Double, Double)
  wrapped = WrappedWidget @ChangeHandler @FloatValue mkFloatSlider
  initializer w (FloatVal d) = setField @FloatValue w d
  initializer w (FloatRange (v, l, u)) = do
    setField @FloatValue w v
    setField @MinFloat w l
    setField @MaxFloat w u
