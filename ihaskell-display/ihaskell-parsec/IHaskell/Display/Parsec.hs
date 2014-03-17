{-# LANGUAGE NoImplicitPrelude, TypeSynonymInstances, QuasiQuotes, FlexibleInstances #-}
module IHaskell.Display.Parsec () where

import ClassyPrelude
import System.Random
import Data.String.Here

import Text.Parsec.String

import IHaskell.Display
import IHaskell.Display (IHaskellWidget)

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
          // Start the Comm.
          var CommManager = IPython.notebook.kernel.comm_manager;
          var comm = CommManager.new_comm("parsec", {}, {
              iopub : {
                output : function () {
                  console.log("Iopub output init:");
                  console.log(arguments);
                }
              }
          });

          // Create the editor.
          var textarea = document.getElementById("${divId}");
          var editor = CodeMirror.fromTextArea(textarea);
          editor.on("change", function() {
            var text = editor.getDoc().getValue();
            console.log("New text: " + text);
            comm.send({"text": text}, function () {
                console.log("Got response!", arguments);
            });
          });
        </script>
        |]

instance IHaskellWidget (Parser a) where
  open widget value publisher = return ()
  comm widget value publisher = return ()
  close widget value = return ()
