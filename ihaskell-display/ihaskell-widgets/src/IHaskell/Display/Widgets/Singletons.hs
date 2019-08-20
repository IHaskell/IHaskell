{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE CPP #-}

module IHaskell.Display.Widgets.Singletons where

import           Data.Singletons.TH

#if MIN_VERSION_singletons(2,4,0)
#else
import           Data.Singletons.Prelude.Ord
#endif

-- Widget properties
singletons
  [d|

  data Field = ViewModule
             | ViewModuleVersion
             | ViewName
             | ModelModule
             | ModelModuleVersion
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
