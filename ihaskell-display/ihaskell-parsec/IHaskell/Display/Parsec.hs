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
        <form><textarea id="parsec-editor">Hello!</textarea></form>
        <!--
        <script>
          // Register the comm target.
          var ParsecWidget = function (comm) {
              this.comm = comm;
              this.comm.on_msg($.proxy(this.handler, this));

              // get the cell that was probably executed
              // msg_id:cell mapping will make this possible without guessing
              this.cell = IPython.notebook.get_cell(IPython.notebook.get_selected_index()-1);
              this.callbacks = {
                  iopub : {
                      output : function () {
                          console.log("Iopub output", arguments);
                      }
                  }
              };

              // Create the editor.
              console.log("Editoring");
              var out = this.cell.output_area.element;
              var textarea = output_area.find("#parsec-editor")[0];
              var editor = CodeMirror.fromTextArea(textarea);
              editor.on("change", function() {
                var text = editor.getDoc().getValue();
                console.log("New text: " + text);
                comm.send({"text": text}, function () {
                    console.log("Got response!", arguments);
                });
              });
          };

          ParsecWidget.prototype.handler = function(msg) {
              console.log('handle', this, msg, this.cell.output_area);
          };

          IPython.notebook.kernel.comm_manager.register_target('parsec', IPython.utils.always_new(ParsecWidget));
        </script>
        -->
        |]

instance IHaskellWidget (Parser a) where
  targetName _ = "parsec"
  open widget value publisher = return ()
  comm widget value publisher = do
    DEAL WITH ACTUAL PARSECS
    publisher value
  close widget value = return ()
