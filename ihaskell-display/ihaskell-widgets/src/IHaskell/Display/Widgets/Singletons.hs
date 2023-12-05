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

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module IHaskell.Display.Widgets.Singletons where

#if MIN_VERSION_singletons(3,0,0)
import           Data.Singletons.Base.TH
import           Data.Eq.Singletons
#elif MIN_VERSION_singletons(2,4,0)
import           Data.Singletons.Prelude.Eq
import           Data.Singletons.TH
#else
import           Data.Singletons.Prelude.Eq
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
             | Titles
             | SelectedIndex
             | ReadOutMsg
             | Indent
             | ContinuousUpdate
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
             | ButtonColor
             | FontWeight
             | DescriptionWidth
             | BarColor
             | HandleColor
             | ButtonWidth
             | Target
             | Source
             | MsgID
             | Outputs
             | Style
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

-- Attributes that aren't synced with the frontend give "" on toKey
promote
  [d|
    -- toKey :: Field -> String
    toKey ViewModule = "_view_module"
    toKey ViewModuleVersion = "_view_module_version"
    toKey ViewName = "_view_name"
    toKey ModelModule = "_model_module"
    toKey ModelModuleVersion = "_model_module_version"
    toKey ModelName = "_model_name"
    toKey DisplayHandler = "" -- Not sent to the frontend
    toKey DOMClasses = "_dom_classes"
    toKey Width = "width"
    toKey Height = "height"
    toKey Description = "description"
    toKey ClickHandler = "" -- Not sent to the frontend
    toKey SubmitHandler = "" -- Not sent to the frontend
    toKey Disabled = "disabled"
    toKey StringValue = "value"
    toKey Placeholder = "placeholder"
    toKey Tooltip = "tooltip"
    toKey Icon = "icon"
    toKey ButtonStyle = "button_style"
    toKey BSValue = "value"
    toKey ImageFormat = "format"
    toKey AudioFormat = "format"
    toKey VideoFormat = "format"
    toKey BoolValue = "value"
    toKey Index = "index"
    toKey OptionalIndex = "index"
    toKey OptionsLabels = "_options_labels"
    toKey SelectionHandler = "" -- Not sent to the frontend
    toKey Tooltips = "tooltips"
    toKey Icons = "icons"
    toKey Indices = "index"
    toKey IntValue = "value"
    toKey StepInt = "step"
    toKey MinInt = "min"
    toKey MaxInt = "max"
    toKey IntPairValue = "value"
    toKey LowerInt = "min"
    toKey UpperInt = "max"
    toKey FloatValue = "value"
    toKey StepFloat = "step"
    toKey MinFloat = "min"
    toKey MaxFloat = "max"
    toKey FloatPairValue = "value"
    toKey LowerFloat = "min"
    toKey UpperFloat = "max"
    toKey Orientation = "orientation"
    toKey BaseFloat = "base"
    toKey ReadOut = "readout"
    toKey ReadOutFormat = "readout_format"
    toKey BarStyle = "bar_style"
    toKey ChangeHandler = "" -- Not sent to the frontend
    toKey Children = "children"
    toKey BoxStyle = "box_style"
    toKey Titles = "_titles"
    toKey SelectedIndex = "selected_index"
    toKey ReadOutMsg = "readout"
    toKey Indent = "indent"
    toKey ContinuousUpdate = "continuous_update"
    toKey Rows = "rows"
    toKey AutoPlay = "autoplay"
    toKey Loop = "loop"
    toKey Controls = "controls"
    toKey Options = "options"
    toKey EnsureOption = "ensure_option"
    toKey Playing = "playing"
    toKey Repeat = "repeat"
    toKey Interval = "interval"
    toKey ShowRepeat = "show_repeat"
    toKey Concise = "concise"
    toKey DateValue = "value"
    toKey Pressed = "pressed"
    toKey Name = "name"
    toKey Mapping = "mapping"
    toKey Connected = "connected"
    toKey Timestamp = "timestamp"
    toKey Buttons = "buttons"
    toKey Axes = "axes"
    toKey Layout = "layout"
    toKey ButtonColor = "button_color"
    toKey FontWeight = "font_weight"
    toKey DescriptionWidth = "description_width"
    toKey BarColor = "bar_color"
    toKey HandleColor = "handle_color"
    toKey ButtonWidth = "button_width"
    toKey Target = "target"
    toKey Source = "source"
    toKey MsgID = "msg_id"
    toKey Outputs = "outputs"
    toKey Style = "style"
    toKey LAlignContent = "align_content"
    toKey LAlignItems = "align_items"
    toKey LAlignSelf = "align_self"
    toKey LBorder = "border"
    toKey LBottom = "bottom"
    toKey LDisplay = "display"
    toKey LFlex = "flex"
    toKey LFlexFlow = "flex_flow"
    toKey LGridArea = "grid_area"
    toKey LGridAutoColumns = "grid_auto_columns"
    toKey LGridAutoFlow = "grid_auto_flow"
    toKey LGridAutoRows = "grid_auto_rows"
    toKey LGridColumn = "grid_column"
    toKey LGridGap = "grid_gap"
    toKey LGridRow = "grid_row"
    toKey LGridTemplateAreas = "grid_template_areas"
    toKey LGridTemplateColumns = "grid_template_columns"
    toKey LGridTemplateRows = "grid_template_rows"
    toKey LHeight = "height"
    toKey LJustifyContent = "justify_content"
    toKey LJustifyItems = "justify_items"
    toKey LLeft = "left"
    toKey LMargin = "margin"
    toKey LMaxHeight = "max_height"
    toKey LMaxWidth = "max_width"
    toKey LMinHeight = "min_height"
    toKey LMinWidth = "min_width"
    toKey LOrder = "order"
    toKey LOverflow = "overflow"
    toKey LOverflowX = "overflow_x"
    toKey LOverflowY = "overflow_y"
    toKey LPadding = "padding"
    toKey LRight = "right"
    toKey LTop = "top"
    toKey LVisibility = "visibility"
    toKey LWidth = "width"

    -- hasKey :: Field -> Bool
    hasKey x = toKey x /= ""
  |]
