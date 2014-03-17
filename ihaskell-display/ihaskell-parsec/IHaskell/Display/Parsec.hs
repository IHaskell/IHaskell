{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, QuasiQuotes, FlexibleInstances #-}
module IHaskell.Display.Parsec () where

import ClassyPrelude
import System.Random
import Data.String.Here

import Text.Parsec.String

import IHaskell.Display

instance IHaskellDisplay (Parser a) where
  display renderable = do
    key <- randomRIO (1, 100000000000) :: IO Int
    return $ Display [html $ dom key]
    where 
      dom key = 
        let divId = "text" ++ show key ++ "" in
        [i|
        <form><textarea id="${divId}">Hello!</textarea></form>
        <script>
          var textarea = document.getElementById("${divId}");
          var editor = CodeMirror.fromTextArea(textarea);
          editor.on("change", function() {
            var text = editor.getDoc().getValue();
            console.log("New text: " + text);
          });
        </script>
        |]
