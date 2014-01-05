{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module IHaskell.Display.Display where

import IHaskell.Display

import Text.Printf

instance Show a => IHaskellDisplay (Maybe a) where
  display just = return [stringDisplay, htmlDisplay]
    where 
      stringDisplay = plain (show just)
      htmlDisplay = html str
      str = case just of
        Nothing -> "<span style='color: red; font-weight: bold;'>Nothing</span>"
        Just x -> printf "<span style='color: green; font-weight: bold;'>Just</span><span style='font-family: monospace;'>%s</span>" (show x)
