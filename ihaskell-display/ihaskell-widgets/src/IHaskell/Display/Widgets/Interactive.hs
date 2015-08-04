{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module IHaskell.Display.Widgets.Interactive (interactive) where

import           Data.Text

import           Data.Vinyl.Lens (type (∈))
import           Data.Vinyl.TypeLevel (RecAll)

import           IHaskell.Display

import           IHaskell.Display.Widgets.Types
import           IHaskell.Display.Widgets.Common
import qualified IHaskell.Display.Widgets.Singletons as S (SField(..), Field(..))

import           IHaskell.Display.Widgets.Box.FlexBox
import           IHaskell.Display.Widgets.String.Text
import           IHaskell.Display.Widgets.Int.BoundedInt.IntSlider
import           IHaskell.Display.Widgets.Output

 
data WrappedWidget w h f a where
        WrappedWidget ::
            (FieldType h ~ IO (), FieldType f ~ a, h ∈ WidgetFields w,
             f ∈ WidgetFields w, ToPairs (Attr h),
             IHaskellWidget (IPythonWidget w)) =>
            IO (IPythonWidget w) ->
              S.SField h -> S.SField f -> WrappedWidget w h f a

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

interactive :: (FromWidget a, IHaskellDisplay b) => (a -> b) -> IO FlexBox
interactive func = do
  let wrap = wrapped
  widget <- construct wrap
  bx <- mkFlexBox
  out <- mkOutputWidget
  setEvent wrap widget $ getValue wrap widget >>= replaceOutput out . func
  trigger wrap widget
  setField out Width 500
  setField bx Orientation VerticalOrientation
  setField bx Children [ChildWidget widget, ChildWidget out]
  return bx
