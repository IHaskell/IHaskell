module Jupyter.IHaskell.Inspect (
  InspectInfo(..),
  inspect,
  )

inspect :: Text -> Int -> Interpreter (Maybe InspectInfo)
