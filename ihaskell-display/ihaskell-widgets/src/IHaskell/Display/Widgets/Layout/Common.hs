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

pattern AlignContent = S.SLAlignContent
pattern AlignItems = S.SLAlignItems
pattern AlignSelf = S.SLAlignSelf
pattern Border = S.SLBorder
pattern Bottom = S.SLBottom
pattern Display = S.SLDisplay
pattern Flex = S.SLFlex
pattern FlexFlow = S.SLFlexFlow
pattern GridArea = S.SLGridArea
pattern GridAutoColumns = S.SLGridAutoColumns
pattern GridAutoFlow = S.SLGridAutoFlow
pattern GridAutoRows = S.SLGridAutoRows
pattern GridColumn = S.SLGridColumn
pattern GridGap = S.SLGridGap
pattern GridRow = S.SLGridRow
pattern GridTemplateAreas = S.SLGridTemplateAreas
pattern GridTemplateColumns = S.SLGridTemplateColumns
pattern GridTemplateRows = S.SLGridTemplateRows
pattern Height = S.SLHeight
pattern JustifyContent = S.SLJustifyContent
pattern JustifyItems = S.SLJustifyItems
pattern Left = S.SLLeft
pattern Margin = S.SLMargin
pattern MaxHeight = S.SLMaxHeight
pattern MaxWidth = S.SLMaxWidth
pattern MinHeight = S.SLMinHeight
pattern MinWidth = S.SLMinWidth
pattern Order = S.SLOrder
pattern Overflow = S.SLOverflow
pattern OverflowX = S.SLOverflowX
pattern OverflowY = S.SLOverflowY
pattern Padding = S.SLPadding
pattern Right = S.SLRight
pattern Top = S.SLTop
pattern Visibility = S.SLVisibility
pattern Width = S.SLWidth

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