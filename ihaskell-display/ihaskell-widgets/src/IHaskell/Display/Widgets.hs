module IHaskell.Display.Widgets (module X) where

import           IHaskell.Display.Widgets.Button as X

import           IHaskell.Display.Widgets.Box.Box as X
import           IHaskell.Display.Widgets.Box.Proxy as X
import           IHaskell.Display.Widgets.Box.PlaceProxy as X
import           IHaskell.Display.Widgets.Box.FlexBox as X
import           IHaskell.Display.Widgets.Box.SelectionContainer.Accordion as X
import           IHaskell.Display.Widgets.Box.SelectionContainer.Tab as X

import           IHaskell.Display.Widgets.Bool.CheckBox as X
import           IHaskell.Display.Widgets.Bool.ToggleButton as X
import           IHaskell.Display.Widgets.Bool.Valid as X

import           IHaskell.Display.Widgets.Int.IntText as X
import           IHaskell.Display.Widgets.Int.BoundedInt.BoundedIntText as X
import           IHaskell.Display.Widgets.Int.BoundedInt.IntProgress as X
import           IHaskell.Display.Widgets.Int.BoundedInt.IntSlider as X
import           IHaskell.Display.Widgets.Int.BoundedIntRange.IntRangeSlider as X

import           IHaskell.Display.Widgets.Float.FloatText as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.BoundedFloatText as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatProgress as X
import           IHaskell.Display.Widgets.Float.BoundedFloat.FloatSlider as X
import           IHaskell.Display.Widgets.Float.BoundedFloatRange.FloatRangeSlider as X

import           IHaskell.Display.Widgets.Image as X

import           IHaskell.Display.Widgets.Output as X

import           IHaskell.Display.Widgets.Selection.Dropdown as X
import           IHaskell.Display.Widgets.Selection.RadioButtons as X
import           IHaskell.Display.Widgets.Selection.Select as X
import           IHaskell.Display.Widgets.Selection.ToggleButtons as X
import           IHaskell.Display.Widgets.Selection.SelectMultiple as X

import           IHaskell.Display.Widgets.String.HTML as X
import           IHaskell.Display.Widgets.String.Latex as X
import           IHaskell.Display.Widgets.String.Text as X
import           IHaskell.Display.Widgets.String.TextArea as X

import           IHaskell.Display.Widgets.Common as X
import           IHaskell.Display.Widgets.Types as X (setField, getField, properties, triggerDisplay,
                                                      triggerChange, triggerClick, triggerSelection,
                                                      triggerSubmit, ChildWidget(..))
