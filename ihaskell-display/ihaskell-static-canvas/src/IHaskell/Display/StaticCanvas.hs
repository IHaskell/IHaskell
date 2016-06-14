{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module IHaskell.Display.StaticCanvas (Canvas(..)) where

import           Data.Text.Lazy.Builder (toLazyText)
import           Data.Text.Lazy (unpack)
import           Data.Text (pack, Text)
import           System.IO.Unsafe
import           Control.Concurrent.MVar

import           Graphics.Static

import           IHaskell.Display

{-# NOINLINE uniqueCounter #-}
uniqueCounter :: MVar Int
uniqueCounter = unsafePerformIO $ newMVar 0

getUniqueName :: IO Text
getUniqueName = do
  val <- takeMVar uniqueCounter
  let val' = val + 1
  putMVar uniqueCounter val'
  return $ pack $ "ihaskellStaticCanvasUniqueID" ++ show val

data Canvas = Canvas { canvasWidth :: Int, canvasHeight :: Int, canvas :: CanvasFree () }

instance IHaskellDisplay Canvas where
  display cnv = do
    name <- getUniqueName
    let script = buildScript' (canvasWidth cnv) (canvasHeight cnv) name (canvas cnv)
    return $ Display [html $ unpack $ toLazyText script]
