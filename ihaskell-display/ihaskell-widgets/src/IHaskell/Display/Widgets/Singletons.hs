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
             | OverflowX
             | OverflowY
             | BoxStyle
             | Flex
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
             deriving (Eq, Ord, Show)
  |]
