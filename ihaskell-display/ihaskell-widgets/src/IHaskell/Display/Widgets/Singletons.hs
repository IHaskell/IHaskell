{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs #-}

module IHaskell.Display.Widgets.Singletons where

import           Data.Singletons.TH
import           Data.Singletons.Prelude.Ord

-- Widget properties
singletons
  [d|
   
  data Field = ViewModule
             | ViewName
             | ModelModule
             | ModelName
             | MsgThrottle
             | Version
             | DisplayHandler
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
             | SubmitHandler
             | Disabled
             | StringValue
             | Placeholder
             | Tooltip
             | Icon
             | ButtonStyle
             | B64Value
             | ImageFormat
             | BoolValue
             | Options
             | SelectedLabel
             | SelectedValue
             | SelectionHandler
             | Tooltips
             | Icons
             | SelectedLabels
             | SelectedValues
             | IntValue
             | StepInt
             | MaxInt
             | MinInt
             | IntPairValue
             | LowerInt
             | UpperInt
             | FloatValue
             | StepFloat
             | MaxFloat
             | MinFloat
             | FloatPairValue
             | LowerFloat
             | UpperFloat
             | Orientation
             | ShowRange
             | ReadOut
             | SliderColor
             | BarStyle
             | ChangeHandler
             | Children
             | OverflowX
             | OverflowY
             | BoxStyle
             | Flex
             | Pack
             | Align
             | Titles
             | SelectedIndex
             | ReadOutMsg
             | Child
             | Selector
             deriving (Eq, Ord, Show)
  |]
