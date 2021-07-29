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

import           Data.Aeson hiding (pairs)
import           Data.List (intercalate)
import           Data.Typeable (Typeable, TypeRep, typeOf)

#if MIN_VERSION_vinyl(0,9,0)
import           Data.Vinyl (Rec(..), Dict(..))
import           Data.Vinyl.Recursive ((<+>), recordToList, reifyConstraint, rmap)
#else
import           Data.Vinyl (Rec(..), (<+>), recordToList, reifyConstraint, rmap, Dict(..))
#endif
import           Data.Vinyl.Lens (rget, rput, type (∈))

#if MIN_VERSION_singletons(3,0,0)
import           Data.List.Singletons
#elif MIN_VERSION_singletons(2,4,0)
import           Data.Singletons.Prelude.List
#else
import           Data.Singletons.Prelude ((:++))
#endif

#if MIN_VERSION_singletons(3,0,0)
import           Data.Singletons.Base.TH
#else
import           Data.Singletons.TH
#endif

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

instance ToPairs (Attr 'S.LAlignContent) where
    toPairs x = ["align_content" .= toJSON x]

instance ToPairs (Attr 'S.LAlignItems) where
    toPairs x = ["align_items" .= toJSON x]

instance ToPairs (Attr 'S.LAlignSelf) where
    toPairs x = ["align_self" .= toJSON x]

instance ToPairs (Attr 'S.LBorder) where
    toPairs x = ["border" .= toJSON x]

instance ToPairs (Attr 'S.LBottom) where
    toPairs x = ["bottom" .= toJSON x]

instance ToPairs (Attr 'S.LDisplay) where
    toPairs x = ["display" .= toJSON x]

instance ToPairs (Attr 'S.LFlex) where
    toPairs x = ["flex" .= toJSON x]

instance ToPairs (Attr 'S.LFlexFlow) where
    toPairs x = ["flex_flow" .= toJSON x]

instance ToPairs (Attr 'S.LGridArea) where
    toPairs x = ["grid_area" .= toJSON x]

instance ToPairs (Attr 'S.LGridAutoColumns) where
    toPairs x = ["grid_auto_columns" .= toJSON x]

instance ToPairs (Attr 'S.LGridAutoFlow) where
    toPairs x = ["grid_auto_flow" .= toJSON x]

instance ToPairs (Attr 'S.LGridAutoRows) where
    toPairs x = ["grid_auto_rows" .= toJSON x]

instance ToPairs (Attr 'S.LGridColumn) where
    toPairs x = ["grid_column" .= toJSON x]

instance ToPairs (Attr 'S.LGridGap) where
    toPairs x = ["grid_gap" .= toJSON x]

instance ToPairs (Attr 'S.LGridRow) where
    toPairs x = ["grid_row" .= toJSON x]

instance ToPairs (Attr 'S.LGridTemplateAreas) where
    toPairs x = ["grid_template_areas" .= toJSON x]

instance ToPairs (Attr 'S.LGridTemplateColumns) where
    toPairs x = ["grid_template_columns" .= toJSON x]

instance ToPairs (Attr 'S.LGridTemplateRows) where
    toPairs x = ["grid_template_rows" .= toJSON x]

instance ToPairs (Attr 'S.LHeight) where
    toPairs x = ["height" .= toJSON x]

instance ToPairs (Attr 'S.LJustifyContent) where
    toPairs x = ["justify_content" .= toJSON x]

instance ToPairs (Attr 'S.LJustifyItems) where
    toPairs x = ["justify_items" .= toJSON x]

instance ToPairs (Attr 'S.LLeft) where
    toPairs x = ["left" .= toJSON x]

instance ToPairs (Attr 'S.LMargin) where
    toPairs x = ["margin" .= toJSON x]

instance ToPairs (Attr 'S.LMaxHeight) where
    toPairs x = ["max_height" .= toJSON x]

instance ToPairs (Attr 'S.LMaxWidth) where
    toPairs x = ["max_width" .= toJSON x]

instance ToPairs (Attr 'S.LMinHeight) where
    toPairs x = ["min_height" .= toJSON x]

instance ToPairs (Attr 'S.LMinWidth) where
    toPairs x = ["min_width" .= toJSON x]

instance ToPairs (Attr 'S.LOrder) where
    toPairs x = ["order" .= toJSON x]

instance ToPairs (Attr 'S.LOverflow) where
    toPairs x = ["overflow" .= toJSON x]

instance ToPairs (Attr 'S.LOverflowX) where
    toPairs x = ["overflow_x" .= toJSON x]

instance ToPairs (Attr 'S.LOverflowY) where
    toPairs x = ["overflow_y" .= toJSON x]

instance ToPairs (Attr 'S.LPadding) where
    toPairs x = ["padding" .= toJSON x]

instance ToPairs (Attr 'S.LRight) where
    toPairs x = ["right" .= toJSON x]

instance ToPairs (Attr 'S.LTop) where
    toPairs x = ["top" .= toJSON x]

instance ToPairs (Attr 'S.LVisibility) where
    toPairs x = ["visibility" .= toJSON x]

instance ToPairs (Attr 'S.LWidth) where
    toPairs x = ["width" .= toJSON x]

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
