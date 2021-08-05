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

module IHaskell.Display.Widgets.Layout.Types where

import           Prelude hiding (Right,Left)

import           Control.Monad (unless)
import qualified Control.Exception as Ex

import           Data.List (intercalate)

import           Data.Vinyl (Rec(..))

import qualified IHaskell.Display.Widgets.Singletons as S
import           IHaskell.Display.Widgets.Types

import           IHaskell.Display.Widgets.Layout.Common

type LayoutClass = [ 'S.ModelModule
                   , 'S.ModelModuleVersion
                   , 'S.ModelName
                   , 'S.ViewModule
                   , 'S.ViewModuleVersion
                   , 'S.ViewName
                   , 'S.LAlignContent
                   , 'S.LAlignItems
                   , 'S.LAlignSelf
                   , 'S.LBorder
                   , 'S.LBottom
                   , 'S.LDisplay
                   , 'S.LFlex
                   , 'S.LFlexFlow
                   , 'S.LGridArea
                   , 'S.LGridAutoColumns
                   , 'S.LGridAutoFlow
                   , 'S.LGridAutoRows
                   , 'S.LGridColumn
                   , 'S.LGridGap
                   , 'S.LGridRow
                   , 'S.LGridTemplateAreas
                   , 'S.LGridTemplateColumns
                   , 'S.LGridTemplateRows
                   , 'S.LHeight
                   , 'S.LJustifyContent
                   , 'S.LJustifyItems
                   , 'S.LLeft
                   , 'S.LMargin
                   , 'S.LMaxHeight
                   , 'S.LMaxWidth
                   , 'S.LMinHeight
                   , 'S.LMinWidth
                   , 'S.LOrder
                   , 'S.LOverflow
                   , 'S.LOverflowX
                   , 'S.LOverflowY
                   , 'S.LPadding
                   , 'S.LRight
                   , 'S.LTop
                   , 'S.LVisibility
                   , 'S.LWidth
                   ]

type instance FieldType 'S.LAlignContent = Maybe String
type instance FieldType 'S.LAlignItems = Maybe String
type instance FieldType 'S.LAlignSelf = Maybe String
type instance FieldType 'S.LBorder = Maybe String
type instance FieldType 'S.LBottom = Maybe String
type instance FieldType 'S.LDisplay = Maybe String
type instance FieldType 'S.LFlex = Maybe String
type instance FieldType 'S.LFlexFlow = Maybe String
type instance FieldType 'S.LGridArea = Maybe String
type instance FieldType 'S.LGridAutoColumns = Maybe String
type instance FieldType 'S.LGridAutoFlow = Maybe String
type instance FieldType 'S.LGridAutoRows = Maybe String
type instance FieldType 'S.LGridColumn = Maybe String
type instance FieldType 'S.LGridGap = Maybe String
type instance FieldType 'S.LGridRow = Maybe String
type instance FieldType 'S.LGridTemplateAreas = Maybe String
type instance FieldType 'S.LGridTemplateColumns = Maybe String
type instance FieldType 'S.LGridTemplateRows = Maybe String
type instance FieldType 'S.LHeight = Maybe String
type instance FieldType 'S.LJustifyContent = Maybe String
type instance FieldType 'S.LJustifyItems = Maybe String
type instance FieldType 'S.LLeft = Maybe String
type instance FieldType 'S.LMargin = Maybe String
type instance FieldType 'S.LMaxHeight = Maybe String
type instance FieldType 'S.LMaxWidth = Maybe String
type instance FieldType 'S.LMinHeight = Maybe String
type instance FieldType 'S.LMinWidth = Maybe String
type instance FieldType 'S.LOrder = Maybe String
type instance FieldType 'S.LOverflow = Maybe String
type instance FieldType 'S.LOverflowX = Maybe String
type instance FieldType 'S.LOverflowY = Maybe String
type instance FieldType 'S.LPadding = Maybe String
type instance FieldType 'S.LRight = Maybe String
type instance FieldType 'S.LTop = Maybe String
type instance FieldType 'S.LVisibility = Maybe String
type instance FieldType 'S.LWidth = Maybe String

-- type family WidgetFields (w :: WidgetType) :: [Field] where
type instance WidgetFields 'LayoutType = LayoutClass

-- | A record representing a widget of the Layour class from IPython
defaultLayoutWidget :: Rec Attr LayoutClass
defaultLayoutWidget = (S.SModelModule =:! "@jupyter-widgets/base")
                      :& (S.SModelModuleVersion =:! "1.1.0")
                      :& (S.SModelName =:! "LayoutModel")
                      :& (S.SViewModule =:! "@jupyter-widgets/base")
                      :& (S.SViewModuleVersion =:! "1.1.0")
                      :& (S.SViewName =:! "LayoutView")
                      :& (AlignContent =:. (Nothing, venum alignContentProps))
                      :& (AlignItems =:. (Nothing, venum alignItemProps))
                      :& (AlignSelf =:. (Nothing, venum alignSelfProps))
                      :& (Border =:: Nothing)
                      :& (Bottom =:: Nothing)
                      :& (Display =:: Nothing)
                      :& (Flex =:: Nothing)
                      :& (FlexFlow =:: Nothing)
                      :& (GridArea =:: Nothing)
                      :& (GridAutoColumns =:: Nothing)
                      :& (GridAutoFlow =:. (Nothing, venum gridAutoFlowProps))
                      :& (GridAutoRows =:: Nothing)
                      :& (GridColumn =:: Nothing)
                      :& (GridGap =:: Nothing)
                      :& (GridRow =:: Nothing)
                      :& (GridTemplateAreas =:: Nothing)
                      :& (GridTemplateColumns =:: Nothing)
                      :& (GridTemplateRows =:: Nothing)
                      :& (Height =:: Nothing)
                      :& (JustifyContent =:: Nothing)
                      :& (JustifyItems =:: Nothing)
                      :& (Left =:: Nothing)
                      :& (Margin =:: Nothing)
                      :& (MaxHeight =:: Nothing)
                      :& (MaxWidth =:: Nothing)
                      :& (MinHeight =:: Nothing)
                      :& (MinWidth =:: Nothing)
                      :& (Order =:: Nothing)
                      :& (Overflow =:. (Nothing, venum overflowProps))
                      :& (OverflowX =:. (Nothing, venum overflowProps))
                      :& (OverflowY =:. (Nothing, venum overflowProps))
                      :& (Padding =:: Nothing)
                      :& (Right =:: Nothing)
                      :& (Top =:: Nothing)
                      :& (Visibility =:. (Nothing, venum visibilityProps))
                      :& (Width =:: Nothing)
                      :& RNil
    where venum :: [String] -> Maybe String -> IO (Maybe String)
          venum _ Nothing = return Nothing
          venum xs (Just f) = do
            unless (f `elem` xs) (Ex.throw $ Ex.AssertionFailed ("The value should be one of: " ++ intercalate ", " xs))
            return $ Just f
