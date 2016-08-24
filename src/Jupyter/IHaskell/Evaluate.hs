module Jupyter.IHaskell.Evaluate (
  Output(..),
  WidgetMessage(..),
  EvalSettings(..),
  eval,
  )

data Output = OutputStdout Text
            | OutputStderr Text
            | OutputDisplay (Map DisplayType ByteString)
            | OutputClear
  deriving (Eq, Ord, Show)

data WidgetMessage = Widget

data EvalSettings =
       EvalSettings
         { evalAllowWidgets :: Bool
         , evalAllowRichDisplays :: Bool
         , evalGetStdin :: Maybe (IO Text)
         , evalHandleOutput :: Output -> IO ()
         , evalHandleWidgetComm :: Maybe (WidgetMessage -> IO ())
         }

eval :: EvalSettings -> Text -> Interpreter Output
