{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- There are lots of pattern synpnyms, and little would be gained by adding
-- the type signatures.
{-# OPTIONS_GHC -fno-warn-missing-pattern-synonym-signatures #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module IHaskell.Display.Widgets.Layout.Common where

import qualified IHaskell.Display.Widgets.Singletons as S

type AlignContent = S.LAlignContent
type AlignItems = S.LAlignItems
type AlignSelf = S.LAlignSelf
type BorderBottom = S.LBorderBottom
type BorderLeft = S.LBorderLeft
type BorderRight = S.LBorderRight
type BorderTop = S.LBorderTop
type Bottom = S.LBottom
type Display = S.LDisplay
type Flex = S.LFlex
type FlexFlow = S.LFlexFlow
type GridArea = S.LGridArea
type GridAutoColumns = S.LGridAutoColumns
type GridAutoFlow = S.LGridAutoFlow
type GridAutoRows = S.LGridAutoRows
type GridColumn = S.LGridColumn
type GridGap = S.LGridGap
type GridRow = S.LGridRow
type GridTemplateAreas = S.LGridTemplateAreas
type GridTemplateColumns = S.LGridTemplateColumns
type GridTemplateRows = S.LGridTemplateRows
type Height = S.LHeight
type JustifyContent = S.LJustifyContent
type JustifyItems = S.LJustifyItems
type Left = S.LLeft
type Margin = S.LMargin
type MaxHeight = S.LMaxHeight
type MaxWidth = S.LMaxWidth
type MinHeight = S.LMinHeight
type MinWidth = S.LMinWidth
type ObjectFit = S.LObjectFit
type ObjectPosition = S.LObjectPosition
type Order = S.LOrder
type Overflow = S.LOverflow
type Padding = S.LPadding
type Right = S.LRight
type Top = S.LTop
type Visibility = S.LVisibility
type Width = S.LWidth

-- TODO: This should be implemented with static type checking, so it's
-- easier to verify at compile-time. "The Haskell Way".
-- But a lot of these fields have common values. ¿Maybe doing some kind
-- of singleton for the CSS fields? ¿Maybe appending the type like
-- InheritOverflow / InheritVisible / InheritGrid...
-- In the meantime we'll use arrays of strings and some runtime verification
cssProps :: [String]
cssProps = ["inherit", "initial", "unset"]
alignContentProps = ["flex-start", "flex-end", "center", "space-between", "space-around", "space-evenly", "stretch"] ++ cssProps
alignItemProps =  ["flex-start", "flex-end", "center", "baseline", "stretch"] ++ cssProps
alignSelfProps = ["auto", "flex-start", "flex-end", "center", "baseline", "stretch"] ++ cssProps
gridAutoFlowProps = ["column", "row", "row dense", "column dense"] ++ cssProps
justifyContentProps = ["flex-start", "flex-end", "center", "space-between", "space-around"] ++ cssProps
justifyItemsProps = ["flex-start", "flex-end", "center"] ++ cssProps
overflowProps = ["visible", "hidden", "scroll", "auto"] ++ cssProps
visibilityProps = ["visible", "hidden"] ++ cssProps
