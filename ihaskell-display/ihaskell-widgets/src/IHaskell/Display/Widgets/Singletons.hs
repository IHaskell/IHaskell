{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module IHaskell.Display.Widgets.Singletons where
import           Data.Typeable (Typeable)

-- Separate types for each Field, no constructors needed
data ViewModule
data ViewModuleVersion
data ViewName
data ModelModule
data ModelModuleVersion
data ModelName
data DisplayHandler
data DOMClasses
data Layout
data Width
data Height
data Description
data DescriptionAllowHtml
data ClickHandler
data SubmitHandler
data Disabled
data StringValue
data Placeholder
data Tooltip
data Tabbable
data Icon
data ButtonStyleField
data BSValue
data ImageFormat
data BoolValue
data OptionsLabels
data Index
data OptionalIndex
data SelectionHandler
data Tooltips
data Icons
data Indices
data IntValue
data StepInt
data MaxInt
data MinInt
data IntPairValue
data LowerInt
data UpperInt
data FloatValue
data StepFloat
data MaxFloat
data MinFloat
data FloatPairValue
data LowerFloat
data UpperFloat
data Orientation
data BaseFloat
data ReadOut
data ReadOutFormat
data BarStyle
data ChangeHandler
data Children
data BoxStyle
data Titles
data SelectedIndex
data ReadOutMsg
data Indent
data ContinuousUpdate
data Rows
data AudioFormat
data VideoFormat
data AutoPlay
data Loop
data Controls
data Options
data EnsureOption
data Playing
data Repeat
data Interval
data ShowRepeat
data Concise
data DateValue
data Pressed
data Name
data Mapping
data Connected
data Timestamp
data Buttons
data Axes
data ButtonColor
data FontFamily
data FontStyle
data FontSize
data FontVariant
data FontWeight
data TextColor
data TextDecoration
data DescriptionWidth
data BarColor
data HandleColor
data ButtonWidth
data Target
data Source
data MsgID
data Outputs
data Style

-- Layout fields with an 'L' prefix
data LAlignContent
data LAlignItems
data LAlignSelf
data LBorderBottom
data LBorderLeft
data LBorderRight
data LBorderTop
data LBottom
data LDisplay
data LFlex
data LFlexFlow
data LGridArea
data LGridAutoColumns
data LGridAutoFlow
data LGridAutoRows
data LGridColumn
data LGridGap
data LGridRow
data LGridTemplateAreas
data LGridTemplateColumns
data LGridTemplateRows
data LHeight
data LJustifyContent
data LJustifyItems
data LLeft
data LMargin
data LMaxHeight
data LMaxWidth
data LMinHeight
data LMinWidth
data LObjectFit
data LObjectPosition
data LOrder
data LOverflow
data LPadding
data LRight
data LTop
data LVisibility
data LWidth

{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Define a class with an associated type family HasKey
class Typeable a => ToKey a where
    toKey :: String
    type HasKey a :: Bool

-- Implementing instances for each field

instance ToKey ViewModule where
    toKey = "_view_module"
    type HasKey ViewModule = 'True

instance ToKey ViewModuleVersion where
    toKey = "_view_module_version"
    type HasKey ViewModuleVersion = 'True

instance ToKey ViewName where
    toKey = "_view_name"
    type HasKey ViewName = 'True

instance ToKey ModelModule where
    toKey = "_model_module"
    type HasKey ModelModule = 'True

instance ToKey ModelModuleVersion where
    toKey = "_model_module_version"
    type HasKey ModelModuleVersion = 'True

instance ToKey ModelName where
    toKey = "_model_name"
    type HasKey ModelName = 'True

instance ToKey DisplayHandler where
    toKey = ""  -- Not sent to the frontend
    type HasKey DisplayHandler = 'False

instance ToKey DOMClasses where
    toKey = "_dom_classes"
    type HasKey DOMClasses = 'True

instance ToKey Width where
    toKey = "width"
    type HasKey Width = 'True

instance ToKey Height where
    toKey = "height"
    type HasKey Height = 'True

instance ToKey Description where
    toKey = "description"
    type HasKey Description = 'True

instance ToKey DescriptionAllowHtml where
    toKey = "description_allow_html"
    type HasKey DescriptionAllowHtml = 'True

instance ToKey ClickHandler where
    toKey = ""  -- Not sent to the frontend
    type HasKey ClickHandler = 'False

instance ToKey SubmitHandler where
    toKey = ""  -- Not sent to the frontend
    type HasKey SubmitHandler = 'False

instance ToKey Disabled where
    toKey = "disabled"
    type HasKey Disabled = 'True

instance ToKey StringValue where
    toKey = "value"
    type HasKey StringValue = 'True

instance ToKey Placeholder where
    toKey = "placeholder"
    type HasKey Placeholder = 'True

instance ToKey Tooltip where
    toKey = "tooltip"
    type HasKey Tooltip = 'True

instance ToKey Tabbable where
    toKey = "tabbable"
    type HasKey Tabbable = 'True

instance ToKey Icon where
    toKey = "icon"
    type HasKey Icon = 'True

instance ToKey ButtonStyleField where
    toKey = "button_style"
    type HasKey ButtonStyleField = 'True

instance ToKey BSValue where
    toKey = "value"
    type HasKey BSValue = 'True

instance ToKey ImageFormat where
    toKey = "format"
    type HasKey ImageFormat = 'True

instance ToKey AudioFormat where
    toKey = "format"
    type HasKey AudioFormat = 'True

instance ToKey VideoFormat where
    toKey = "format"
    type HasKey VideoFormat = 'True

instance ToKey BoolValue where
    toKey = "value"
    type HasKey BoolValue = 'True

instance ToKey Index where
    toKey = "index"
    type HasKey Index = 'True

instance ToKey OptionalIndex where
    toKey = "index"
    type HasKey OptionalIndex = 'True

instance ToKey OptionsLabels where
    toKey = "_options_labels"
    type HasKey OptionsLabels = 'True

instance ToKey SelectionHandler where
    toKey = ""  -- Not sent to the frontend
    type HasKey SelectionHandler = 'False

instance ToKey Tooltips where
    toKey = "tooltips"
    type HasKey Tooltips = 'True

instance ToKey Icons where
    toKey = "icons"
    type HasKey Icons = 'True

instance ToKey Indices where
    toKey = "index"
    type HasKey Indices = 'True

instance ToKey IntValue where
    toKey = "value"
    type HasKey IntValue = 'True

instance ToKey StepInt where
    toKey = "step"
    type HasKey StepInt = 'True

instance ToKey MinInt where
    toKey = "min"
    type HasKey MinInt = 'True

instance ToKey MaxInt where
    toKey = "max"
    type HasKey MaxInt = 'True

instance ToKey IntPairValue where
    toKey = "value"
    type HasKey IntPairValue = 'True

instance ToKey LowerInt where
    toKey = "min"
    type HasKey LowerInt = 'True

instance ToKey UpperInt where
    toKey = "max"
    type HasKey UpperInt = 'True

instance ToKey FloatValue where
    toKey = "value"
    type HasKey FloatValue = 'True

instance ToKey StepFloat where
    toKey = "step"
    type HasKey StepFloat = 'True

instance ToKey MinFloat where
    toKey = "min"
    type HasKey MinFloat = 'True

instance ToKey MaxFloat where
    toKey = "max"
    type HasKey MaxFloat = 'True

instance ToKey FloatPairValue where
    toKey = "value"
    type HasKey FloatPairValue = 'True

instance ToKey LowerFloat where
    toKey = "min"
    type HasKey LowerFloat = 'True

instance ToKey UpperFloat where
    toKey = "max"
    type HasKey UpperFloat = 'True

instance ToKey Orientation where
    toKey = "orientation"
    type HasKey Orientation = 'True

instance ToKey BaseFloat where
    toKey = "base"
    type HasKey BaseFloat = 'True

instance ToKey ReadOut where
    toKey = "readout"
    type HasKey ReadOut = 'True

instance ToKey ReadOutFormat where
    toKey = "readout_format"
    type HasKey ReadOutFormat = 'True

instance ToKey BarStyle where
    toKey = "bar_style"
    type HasKey BarStyle = 'True

instance ToKey ChangeHandler where
    toKey = ""  -- Not sent to the frontend
    type HasKey ChangeHandler = 'False

instance ToKey Children where
    toKey = "children"
    type HasKey Children = 'True

instance ToKey BoxStyle where
    toKey = "box_style"
    type HasKey BoxStyle = 'True

instance ToKey Titles where
    toKey = "titles"
    type HasKey Titles = 'True

instance ToKey SelectedIndex where
    toKey = "selected_index"
    type HasKey SelectedIndex = 'True

instance ToKey ReadOutMsg where
    toKey = "readout"
    type HasKey ReadOutMsg = 'True

instance ToKey Indent where
    toKey = "indent"
    type HasKey Indent = 'True

instance ToKey ContinuousUpdate where
    toKey = "continuous_update"
    type HasKey ContinuousUpdate = 'True

instance ToKey Rows where
    toKey = "rows"
    type HasKey Rows = 'True

instance ToKey AutoPlay where
    toKey = "autoplay"
    type HasKey AutoPlay = 'True

instance ToKey Loop where
    toKey = "loop"
    type HasKey Loop = 'True

instance ToKey Controls where
    toKey = "controls"
    type HasKey Controls = 'True

instance ToKey Options where
    toKey = "options"
    type HasKey Options = 'True

instance ToKey EnsureOption where
    toKey = "ensure_option"
    type HasKey EnsureOption = 'True

instance ToKey Playing where
    toKey = "playing"
    type HasKey Playing = 'True

instance ToKey Repeat where
    toKey = "repeat"
    type HasKey Repeat = 'True

instance ToKey Interval where
    toKey = "interval"
    type HasKey Interval = 'True

instance ToKey ShowRepeat where
    toKey = "show_repeat"
    type HasKey ShowRepeat = 'True

instance ToKey Concise where
    toKey = "concise"
    type HasKey Concise = 'True

instance ToKey DateValue where
    toKey = "value"
    type HasKey DateValue = 'True

instance ToKey Pressed where
    toKey = "pressed"
    type HasKey Pressed = 'True

instance ToKey Name where
    toKey = "name"
    type HasKey Name = 'True

instance ToKey Mapping where
    toKey = "mapping"
    type HasKey Mapping = 'True

instance ToKey Connected where
    toKey = "connected"
    type HasKey Connected = 'True

instance ToKey Timestamp where
    toKey = "timestamp"
    type HasKey Timestamp = 'True

instance ToKey Buttons where
    toKey = "buttons"
    type HasKey Buttons = 'True

instance ToKey Axes where
    toKey = "axes"
    type HasKey Axes = 'True

instance ToKey Layout where
    toKey = "layout"
    type HasKey Layout = 'True

instance ToKey ButtonColor where
    toKey = "button_color"
    type HasKey ButtonColor = 'True

instance ToKey FontFamily where
    toKey = "font_family"
    type HasKey FontFamily = 'True

instance ToKey FontSize where
    toKey = "font_size"
    type HasKey FontSize = 'True

instance ToKey FontStyle where
    toKey = "font_style"
    type HasKey FontStyle = 'True

instance ToKey FontVariant where
    toKey = "font_variant"
    type HasKey FontVariant = 'True

instance ToKey FontWeight where
    toKey = "font_weight"
    type HasKey FontWeight = 'True

instance ToKey TextColor where
    toKey = "text_color"
    type HasKey TextColor = 'True

instance ToKey TextDecoration where
    toKey = "text_decoration"
    type HasKey TextDecoration = 'True

instance ToKey DescriptionWidth where
    toKey = "description_width"
    type HasKey DescriptionWidth = 'True

instance ToKey BarColor where
    toKey = "bar_color"
    type HasKey BarColor = 'True

instance ToKey HandleColor where
    toKey = "handle_color"
    type HasKey HandleColor = 'True

instance ToKey ButtonWidth where
    toKey = "button_width"
    type HasKey ButtonWidth = 'True

instance ToKey Target where
    toKey = "target"
    type HasKey Target = 'True

instance ToKey Source where
    toKey = "source"
    type HasKey Source = 'True

instance ToKey MsgID where
    toKey = "msg_id"
    type HasKey MsgID = 'True

instance ToKey Outputs where
    toKey = "outputs"
    type HasKey Outputs = 'True

instance ToKey Style where
    toKey = "style"
    type HasKey Style = 'True

-- Layout fields
instance ToKey LAlignContent where
    toKey = "align_content"
    type HasKey LAlignContent = 'True

instance ToKey LAlignItems where
    toKey = "align_items"
    type HasKey LAlignItems = 'True

instance ToKey LAlignSelf where
    toKey = "align_self"
    type HasKey LAlignSelf = 'True

instance ToKey LBorderBottom where
    toKey = "border_bottom"
    type HasKey LBorderBottom = 'True

instance ToKey LBorderLeft where
    toKey = "border_left"
    type HasKey LBorderLeft = 'True

instance ToKey LBorderRight where
    toKey = "border_right"
    type HasKey LBorderRight = 'True

instance ToKey LBorderTop where
    toKey = "border_top"
    type HasKey LBorderTop = 'True

instance ToKey LBottom where
    toKey = "bottom"
    type HasKey LBottom = 'True

instance ToKey LDisplay where
    toKey = "display"
    type HasKey LDisplay = 'True

instance ToKey LFlex where
    toKey = "flex"
    type HasKey LFlex = 'True

instance ToKey LFlexFlow where
    toKey = "flex_flow"
    type HasKey LFlexFlow = 'True

instance ToKey LGridArea where
    toKey = "grid_area"
    type HasKey LGridArea = 'True

instance ToKey LGridAutoColumns where
    toKey = "grid_auto_columns"
    type HasKey LGridAutoColumns = 'True

instance ToKey LGridAutoFlow where
    toKey = "grid_auto_flow"
    type HasKey LGridAutoFlow = 'True

instance ToKey LGridAutoRows where
    toKey = "grid_auto_rows"
    type HasKey LGridAutoRows = 'True

instance ToKey LGridColumn where
    toKey = "grid_column"
    type HasKey LGridColumn = 'True

instance ToKey LGridGap where
    toKey = "grid_gap"
    type HasKey LGridGap = 'True

instance ToKey LGridRow where
    toKey = "grid_row"
    type HasKey LGridRow = 'True

instance ToKey LGridTemplateAreas where
    toKey = "grid_template_areas"
    type HasKey LGridTemplateAreas = 'True

instance ToKey LGridTemplateColumns where
    toKey = "grid_template_columns"
    type HasKey LGridTemplateColumns = 'True

instance ToKey LGridTemplateRows where
    toKey = "grid_template_rows"
    type HasKey LGridTemplateRows = 'True

instance ToKey LHeight where
    toKey = "height"
    type HasKey LHeight = 'True

instance ToKey LJustifyContent where
    toKey = "justify_content"
    type HasKey LJustifyContent = 'True

instance ToKey LJustifyItems where
    toKey = "justify_items"
    type HasKey LJustifyItems = 'True

instance ToKey LLeft where
    toKey = "left"
    type HasKey LLeft = 'True

instance ToKey LMargin where
    toKey = "margin"
    type HasKey LMargin = 'True

instance ToKey LMaxHeight where
    toKey = "max_height"
    type HasKey LMaxHeight = 'True

instance ToKey LMaxWidth where
    toKey = "max_width"
    type HasKey LMaxWidth = 'True

instance ToKey LMinHeight where
    toKey = "min_height"
    type HasKey LMinHeight = 'True

instance ToKey LMinWidth where
    toKey = "min_width"
    type HasKey LMinWidth = 'True

instance ToKey LObjectFit where
    toKey = "object_fit"
    type HasKey LObjectFit = 'True

instance ToKey LObjectPosition where
    toKey = "object_position"
    type HasKey LObjectPosition = 'True

instance ToKey LOrder where
    toKey = "order"
    type HasKey LOrder = 'True

instance ToKey LOverflow where
    toKey = "overflow"
    type HasKey LOverflow = 'True

instance ToKey LPadding where
    toKey = "padding"
    type HasKey LPadding = 'True

instance ToKey LRight where
    toKey = "right"
    type HasKey LRight = 'True

instance ToKey LTop where
    toKey = "top"
    type HasKey LTop = 'True

instance ToKey LVisibility where
    toKey = "visibility"
    type HasKey LVisibility = 'True

instance ToKey LWidth where
    toKey = "width"
    type HasKey LWidth = 'True


hasKey :: forall a. ToKey a => Bool
hasKey = toKey @a /= ""
