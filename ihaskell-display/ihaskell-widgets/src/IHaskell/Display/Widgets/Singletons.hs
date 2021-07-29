{-# LANGUAGE CPP #-}
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
{-# LANGUAGE TypeApplications #-}
#if __GLASGOW_HASKELL__ >= 810
{-# LANGUAGE StandaloneKindSignatures #-}
#endif

module IHaskell.Display.Widgets.Singletons where

import           Data.Kind

#if MIN_VERSION_singletons(3,0,0)
import           Data.Singletons.Base.TH
#elif MIN_VERSION_singletons(2,4,0)
import           Data.Singletons.TH
#else
import           Data.Singletons.Prelude.Ord
import           Data.Singletons.TH
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
             | DisplayHandler
             | DOMClasses
             | Layout
             | Width
             | Height
             | Description
             | ClickHandler
             | SubmitHandler
             | Disabled
             | StringValue
             | Placeholder
             | Tooltip
             | Icon
             | ButtonStyle
             | BSValue
             | ImageFormat
             | BoolValue
             | OptionsLabels
             | Index
             | OptionalIndex
             | SelectionHandler
             | Tooltips
             | Icons
             | Indices
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
             | BaseFloat
             | ReadOut
             | ReadOutFormat
             | BarStyle
             | ChangeHandler
             | Children
             | BoxStyle
             | Pack
             | Align
             | Titles
             | SelectedIndex
             | ReadOutMsg
             | Indent
             | Child
             | Selector
             | ContinuousUpdate
             | Tabbable
             | Rows
             | AudioFormat
             | VideoFormat
             | AutoPlay
             | Loop
             | Controls
             | Options
             | EnsureOption
             | Playing
             | Repeat
             | Interval
             | ShowRepeat
             | Concise
             | DateValue
             | Pressed
             | Name
             | Mapping
             | Connected
             | Timestamp
             | Buttons
             | Axes
             -- Now the ones for layout
             -- Every layout property comes with an L before the name to avoid conflict
             -- The patterns from Layout.Common remove that leading L
             | LAlignContent
             | LAlignItems
             | LAlignSelf
             | LBorder
             | LBottom
             | LDisplay
             | LFlex
             | LFlexFlow
             | LGridArea
             | LGridAutoColumns
             | LGridAutoFlow
             | LGridAutoRows
             | LGridColumn
             | LGridGap
             | LGridRow
             | LGridTemplateAreas
             | LGridTemplateColumns
             | LGridTemplateRows
             | LHeight
             | LJustifyContent
             | LJustifyItems
             | LLeft
             | LMargin
             | LMaxHeight
             | LMaxWidth
             | LMinHeight
             | LMinWidth
             | LOrder
             | LOverflow
             | LOverflowX
             | LOverflowY
             | LPadding
             | LRight
             | LTop
             | LVisibility
             | LWidth
             deriving (Eq, Ord, Show)
  |]
