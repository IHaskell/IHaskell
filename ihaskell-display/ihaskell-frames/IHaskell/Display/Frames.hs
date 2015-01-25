{-# LANGUAGE TypeSynonymInstances 
            , FlexibleInstances 
            , UndecidableInstances 
            , RankNTypes 
            , DataKinds
            , ConstraintKinds
            , KindSignatures #-} 

module IHaskell.Display.Frames () where

import           Control.Monad (forM_, mapM_)
import           Data.Text                      as DT  hiding (map) 
import           Data.Text.Lazy                 as DTL hiding (map)  
import           Data.Vinyl.Functor             as DVF
import           Data.Vinyl.TypeLevel (RecAll)
import           Frames
import           IHaskell.Display
import           IHaskell.Types
import           Text.Blaze.Html.Renderer.Text  as RT 
import           Text.Blaze.Html5 (table, em, td, tr, th, toHtml, ToMarkup, Html)
import qualified Data.Foldable                  as F

frameToHtml :: forall (ts :: [*]) (t :: * -> *). (RecAll DVF.Identity (UnColumn ts) Show, ColumnHeaders ts, AsVinyl ts, F.Foldable t)            => t (RecF DVF.Identity ts) -> Html
frameToHtml fr = let headerRow = columnHeaders fr 
                     fields = map showFields (F.toList fr) 
                 in 
                     table $ do mapM_ (th . toHtml) headerRow
                                forM_ fields (tr . mapM_ (td . toHtml))

recToHtml :: (RecAll DVF.Identity ts Show, ColumnHeaders ts, AsVinyl ts) => (RecF DVF.Identity ts) -> Html
recToHtml rec = toHtml $ show rec

displayHtml f = let h = (DT.concat . DTL.toChunks . RT.renderHtml) f
                in 
                display $ DisplayData MimeHtml h  

-- This instance matches a whole Frame for display as an HTML table, eg: Frame User 
instance (RecAll DVF.Identity (UnColumn ts) Show, ColumnHeaders ts, AsVinyl ts) => IHaskellDisplay (Frame (RecF DVF.Identity ts)) where
     display mm = displayHtml $ frameToHtml mm

-- This instance matches a record within a frame, eg: User 
instance (RecAll DVF.Identity ts Show, AsVinyl ts, ColumnHeaders ts) => IHaskellDisplay (RecF DVF.Identity ts) where
     display mm = displayHtml $ recToHtml mm  

