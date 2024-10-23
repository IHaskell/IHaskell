{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module IHaskell.Display.Widgets.Layout.Types where

import           Prelude hiding (Right,Left)

import           Control.Monad (unless)
import qualified Control.Exception as Ex

import           Data.List (intercalate)

import           Data.Vinyl (Rec(..))

import qualified IHaskell.Display.Widgets.Singletons as S
import           IHaskell.Display.Widgets.Types

import           IHaskell.Display.Widgets.Layout.Common

type LayoutClass = [ S.ModelModule
                   , S.ModelModuleVersion
                   , S.ModelName
                   , S.ViewModule
                   , S.ViewModuleVersion
                   , S.ViewName
                   , S.LAlignContent
                   , S.LAlignItems
                   , S.LAlignSelf
                   , S.LBorderBottom
                   , S.LBorderLeft
                   , S.LBorderRight
                   , S.LBorderTop
                   , S.LBottom
                   , S.LDisplay
                   , S.LFlex
                   , S.LFlexFlow
                   , S.LGridArea
                   , S.LGridAutoColumns
                   , S.LGridAutoFlow
                   , S.LGridAutoRows
                   , S.LGridColumn
                   , S.LGridGap
                   , S.LGridRow
                   , S.LGridTemplateAreas
                   , S.LGridTemplateColumns
                   , S.LGridTemplateRows
                   , S.LHeight
                   , S.LJustifyContent
                   , S.LJustifyItems
                   , S.LLeft
                   , S.LMargin
                   , S.LMaxHeight
                   , S.LMaxWidth
                   , S.LMinHeight
                   , S.LMinWidth
                   , S.LObjectFit
                   , S.LObjectPosition
                   , S.LOrder
                   , S.LOverflow
                   , S.LPadding
                   , S.LRight
                   , S.LTop
                   , S.LVisibility
                   , S.LWidth
                   ]

type instance FieldType S.LAlignContent = Maybe String
type instance FieldType S.LAlignItems = Maybe String
type instance FieldType S.LAlignSelf = Maybe String
type instance FieldType S.LBorderBottom = Maybe String
type instance FieldType S.LBorderLeft = Maybe String
type instance FieldType S.LBorderRight = Maybe String
type instance FieldType S.LBorderTop = Maybe String
type instance FieldType S.LBottom = Maybe String
type instance FieldType S.LDisplay = Maybe String
type instance FieldType S.LFlex = Maybe String
type instance FieldType S.LFlexFlow = Maybe String
type instance FieldType S.LGridArea = Maybe String
type instance FieldType S.LGridAutoColumns = Maybe String
type instance FieldType S.LGridAutoFlow = Maybe String
type instance FieldType S.LGridAutoRows = Maybe String
type instance FieldType S.LGridColumn = Maybe String
type instance FieldType S.LGridGap = Maybe String
type instance FieldType S.LGridRow = Maybe String
type instance FieldType S.LGridTemplateAreas = Maybe String
type instance FieldType S.LGridTemplateColumns = Maybe String
type instance FieldType S.LGridTemplateRows = Maybe String
type instance FieldType S.LHeight = Maybe String
type instance FieldType S.LJustifyContent = Maybe String
type instance FieldType S.LJustifyItems = Maybe String
type instance FieldType S.LLeft = Maybe String
type instance FieldType S.LMargin = Maybe String
type instance FieldType S.LMaxHeight = Maybe String
type instance FieldType S.LMaxWidth = Maybe String
type instance FieldType S.LMinHeight = Maybe String
type instance FieldType S.LMinWidth = Maybe String
type instance FieldType S.LObjectFit = Maybe String
type instance FieldType S.LObjectPosition = Maybe String
type instance FieldType S.LOrder = Maybe String
type instance FieldType S.LOverflow = Maybe String
type instance FieldType S.LPadding = Maybe String
type instance FieldType S.LRight = Maybe String
type instance FieldType S.LTop = Maybe String
type instance FieldType S.LVisibility = Maybe String
type instance FieldType S.LWidth = Maybe String

-- type family WidgetFields w :: [*] where
type instance WidgetFields LayoutType = LayoutClass

-- | A record representing a widget of the Layour class from IPython
defaultLayoutWidget :: Rec Attr LayoutClass
defaultLayoutWidget = (   F @S.ModelModule =:! "@jupyter-widgets/base")
                      :& (F @S.ModelModuleVersion =:! "2.0.0")
                      :& (F @S.ModelName =:! "LayoutModel")
                      :& (F @S.ViewModule =:! "@jupyter-widgets/base")
                      :& (F @S.ViewModuleVersion =:! "2.0.0")
                      :& (F @S.ViewName =:! "LayoutView")
                      :& (F @AlignContent =:. (Nothing, venum alignContentProps))
                      :& (F @AlignItems =:. (Nothing, venum alignItemProps))
                      :& (F @AlignSelf =:. (Nothing, venum alignSelfProps))
                      :& (F @BorderBottom =:: Nothing)
                      :& (F @BorderLeft =:: Nothing)
                      :& (F @BorderRight =:: Nothing)
                      :& (F @BorderTop =:: Nothing)
                      :& (F @Bottom =:: Nothing)
                      :& (F @Display =:: Nothing)
                      :& (F @Flex =:: Nothing)
                      :& (F @FlexFlow =:: Nothing)
                      :& (F @GridArea =:: Nothing)
                      :& (F @GridAutoColumns =:: Nothing)
                      :& (F @GridAutoFlow =:. (Nothing, venum gridAutoFlowProps))
                      :& (F @GridAutoRows =:: Nothing)
                      :& (F @GridColumn =:: Nothing)
                      :& (F @GridGap =:: Nothing)
                      :& (F @GridRow =:: Nothing)
                      :& (F @GridTemplateAreas =:: Nothing)
                      :& (F @GridTemplateColumns =:: Nothing)
                      :& (F @GridTemplateRows =:: Nothing)
                      :& (F @Height =:: Nothing)
                      :& (F @JustifyContent =:: Nothing)
                      :& (F @JustifyItems =:: Nothing)
                      :& (F @Left =:: Nothing)
                      :& (F @Margin =:: Nothing)
                      :& (F @MaxHeight =:: Nothing)
                      :& (F @MaxWidth =:: Nothing)
                      :& (F @MinHeight =:: Nothing)
                      :& (F @MinWidth =:: Nothing)
                      :& (F @ObjectFit =:: Nothing)
                      :& (F @ObjectPosition =:: Nothing)
                      :& (F @Order =:: Nothing)
                      :& (F @Overflow =:. (Nothing, venum overflowProps))
                      :& (F @Padding =:: Nothing)
                      :& (F @Right =:: Nothing)
                      :& (F @Top =:: Nothing)
                      :& (F @Visibility =:. (Nothing, venum visibilityProps))
                      :& (F @Width =:: Nothing)
                      :& RNil
    where venum :: [String] -> Maybe String -> IO (Maybe String)
          venum _ Nothing = return Nothing
          venum xs (Just f) = do
            unless (f `elem` xs) (Ex.throw $ Ex.AssertionFailed ("The value should be one of: " ++ intercalate ", " xs))
            return $ Just f
