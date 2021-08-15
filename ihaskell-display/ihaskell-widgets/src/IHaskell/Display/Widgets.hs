{-|
Module      : ihaskell-widgets
Description : Jupyter Widgets implementation for the IHaskell kernel
Copyright   : (c) Sumit Shrawat, 2015
                  David Davó, 2021
License     : MIT
Maintainer  : david@ddavo.me
Stability   : experimental
-}
module IHaskell.Display.Widgets (module X) where

import           IHaskell.Display.Widgets.Button as X
import           IHaskell.Display.Widgets.ColorPicker as X
import           IHaskell.Display.Widgets.DatePicker as X

import           IHaskell.Display.Widgets.Box.Box as X
import           IHaskell.Display.Widgets.Box.GridBox as X
import           IHaskell.Display.Widgets.Box.HBox as X
import           IHaskell.Display.Widgets.Box.VBox as X
import           IHaskell.Display.Widgets.Box.SelectionContainer.Accordion as X
import           IHaskell.Display.Widgets.Box.SelectionContainer.Tab as X

import           IHaskell.Display.Widgets.Bool.CheckBox as X
import           IHaskell.Display.Widgets.Bool.ToggleButton as X
import           IHaskell.Display.Widgets.Bool.Valid as X

import           IHaskell.Display.Widgets.Controller.Controller as X
import           IHaskell.Display.Widgets.Controller.ControllerAxis as X
import           IHaskell.Display.Widgets.Controller.ControllerButton as X

import           IHaskell.Display.Widgets.Int.IntText as X
import           IHaskell.Display.Widgets.Int.BoundedInt.BoundedIntText as X
import           IHaskell.Display.Widgets.Int.BoundedInt.IntProgress as X
import           IHaskell.Display.Widgets.Int.BoundedInt.IntSlider as X
import           IHaskell.Display.Widgets.Int.BoundedInt.Play as X
import           IHaskell.Display.Widgets.Int.BoundedIntRange.IntRangeSlider as X

import           IHaskell.Display.Widgets.Link.Link as X
import           IHaskell.Display.Widgets.Link.DirectionalLink as X

import           IHaskell.Display.Widgets.Float.FloatText as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.BoundedFloatText as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatProgress as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatSlider as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatLogSlider as X
import           IHaskell.Display.Widgets.Float.BoundedFloatRange.FloatRangeSlider as X

import           IHaskell.Display.Widgets.Media.Audio as X
import           IHaskell.Display.Widgets.Media.Image as X
import           IHaskell.Display.Widgets.Media.Video as X

import           IHaskell.Display.Widgets.Output as X

import           IHaskell.Display.Widgets.Selection.Dropdown as X
import           IHaskell.Display.Widgets.Selection.RadioButtons as X
import           IHaskell.Display.Widgets.Selection.Select as X
import           IHaskell.Display.Widgets.Selection.SelectionSlider as X
import           IHaskell.Display.Widgets.Selection.SelectionRangeSlider as X
import           IHaskell.Display.Widgets.Selection.ToggleButtons as X
import           IHaskell.Display.Widgets.Selection.SelectMultiple as X

import           IHaskell.Display.Widgets.String.Combobox as X
import           IHaskell.Display.Widgets.String.HTML as X
import           IHaskell.Display.Widgets.String.HTMLMath as X
import           IHaskell.Display.Widgets.String.Label as X
import           IHaskell.Display.Widgets.String.Password as X
import           IHaskell.Display.Widgets.String.Text as X
import           IHaskell.Display.Widgets.String.TextArea as X

import           IHaskell.Display.Widgets.Style.ButtonStyle as X
import           IHaskell.Display.Widgets.Style.DescriptionStyle as X
import           IHaskell.Display.Widgets.Style.ProgressStyle as X
import           IHaskell.Display.Widgets.Style.SliderStyle as X
import           IHaskell.Display.Widgets.Style.ToggleButtonsStyle as X

import           IHaskell.Display.Widgets.Common as X
import           IHaskell.Display.Widgets.Types as X (setField, getField, properties, triggerDisplay,
                                                      triggerChange, triggerClick, triggerSelection,
                                                      triggerSubmit, ChildWidget(..), StyleWidget(..),
                                                      WidgetFieldPair(..), Date(..), unlink,
                                                      JSONByteString(..), OutputMsg(..))

