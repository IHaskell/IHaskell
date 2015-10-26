{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

module IHaskell.Display.Widgets.Interactive (interactive, uncurryHList, Rec(..), Argument(..)) where

import           Data.Text
import           Data.Proxy

import           Data.Vinyl.Core
import           Data.Vinyl.Functor (Identity(..), Const(..))
import           Data.Vinyl.Derived (HList)
import           Data.Vinyl.Lens (type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           IHaskell.Display

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import qualified IHaskell.Display.Widgets.Singletons as S (SField, Field(..))

import           IHaskell.Display.Widgets.Box.FlexBox
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
setInitialValues (Initializer initializer :& fs) (RequiredWidget widget :& ws) (argument :& vs) = do
  initializer widget argument
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
            => (HList ts -> r) -> Rec Argument ts -> IO FlexBox
interactive func =
  let confs = mkConfs Proxy
  in liftToWidgets func confs

-- | Transform a function (HList ts -> r) to one which: 1) Uses widgets to accept the arguments 2)
-- Accepts initial values for the arguments 3) Creates a compound FlexBox widget with an embedded
-- OutputWidget for display
liftToWidgets :: IHaskellDisplay r
              => (HList ts -> r) -> Rec WidgetConf ts -> Rec Argument ts -> IO FlexBox
liftToWidgets func rc initvals = do
  let constructors = rmap extractConstructor rc
      getters = rmap extractGetter rc
      eventSetters = rmap extractEventSetter rc
      initializers = rmap extractInitializer rc

  bx <- mkFlexBox
  out <- mkOutputWidget

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
  setField out Width 500
  setField bx Orientation VerticalOrientation

  -- Set children for the FlexBox
  let children = mkChildren widgets
  setField bx Children $ children ++ [ChildWidget out]

  return bx

 
data WrappedWidget w h f a where
        WrappedWidget ::
            (FieldType h ~ IO (), FieldType f ~ a, h ∈ WidgetFields w,
             f ∈ WidgetFields w, ToPairs (Attr h),
             IHaskellWidget (IPythonWidget w), ToPairs (Attr f)) =>
            IO (IPythonWidget w) ->
              S.SField h -> S.SField f -> WrappedWidget w h f a

construct :: WrappedWidget w h f a -> IO (IPythonWidget w)
construct (WrappedWidget cons _ _) = cons

getValue :: WrappedWidget w h f a -> IPythonWidget w -> IO a
getValue (WrappedWidget _ _ field) widget = getField widget field

setEvent :: WrappedWidget w h f a -> IPythonWidget w -> IO () -> IO ()
setEvent (WrappedWidget _ h _) widget = setField widget h

class RecAll Attr (WidgetFields (SuitableWidget a)) ToPairs => FromWidget a where
  type SuitableWidget a :: WidgetType
  type SuitableHandler a :: S.Field
  type SuitableField a :: S.Field
  data Argument a
  initializer :: IPythonWidget (SuitableWidget a) -> Argument a -> IO ()
  wrapped :: WrappedWidget (SuitableWidget a) (SuitableHandler a) (SuitableField a) a

instance FromWidget Bool where
  type SuitableWidget Bool = CheckBoxType
  type SuitableHandler Bool = S.ChangeHandler
  type SuitableField Bool = S.BoolValue
  data Argument Bool = BoolVal Bool
  initializer w (BoolVal b) = setField w BoolValue b
  wrapped = WrappedWidget mkCheckBox ChangeHandler BoolValue

instance FromWidget Text where
  type SuitableWidget Text = TextType
  type SuitableHandler Text = S.SubmitHandler
  type SuitableField Text = S.StringValue
  data Argument Text = TextVal Text
  initializer w (TextVal txt) = setField w StringValue txt
  wrapped = WrappedWidget mkTextWidget SubmitHandler StringValue

instance FromWidget Integer where
  type SuitableWidget Integer = IntSliderType
  type SuitableHandler Integer = S.ChangeHandler
  type SuitableField Integer = S.IntValue
  data Argument Integer = IntVal Integer
                      | IntRange (Integer, Integer, Integer)
  wrapped = WrappedWidget mkIntSlider ChangeHandler IntValue
  initializer w (IntVal int) = setField w IntValue int
  initializer w (IntRange (v, l, u)) = do
    setField w IntValue v
    setField w MinInt l
    setField w MaxInt u

instance FromWidget Double where
  type SuitableWidget Double = FloatSliderType
  type SuitableHandler Double = S.ChangeHandler
  type SuitableField Double = S.FloatValue
  data Argument Double = FloatVal Double
                     | FloatRange (Double, Double, Double)
  wrapped = WrappedWidget mkFloatSlider ChangeHandler FloatValue
  initializer w (FloatVal d) = setField w FloatValue d
  initializer w (FloatRange (v, l, u)) = do
    setField w FloatValue v
    setField w MinFloat l
    setField w MaxFloat u
