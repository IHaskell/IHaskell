module Simple where

import IHaskell.IPython.EasyKernel (easyKernel, installKernelspec)

functions :: [(String, Int -> Int -> Int)]
functions = [("plus", (+)), ("minus", (-)), ("times", (*)), ("div", div), ("exp", (^))]

languageConfig :: LanguageInfo
languageConfig = LanguageInfo
  { languageName = "funcalc"
  , languageVersion = "1.0.0"
  , languageFileExtension = ".txt"
  , languageCodeMirrorMode = "null"
  }

languageKernelspec :: KernelSpec
languageKernelspec = KernelSpec
  { kernelDisplayName = "Calculator"
  , kernelLanguage = "calc"
  , kernelCommand = ["fun-calc-example", "kernel", "{connection_file}"]
  }

displayString :: String -> [DisplayData]
displayString str = [DisplayData PlainText (T.pack str)]

languageCompletion :: Monad m => T.Text -> Int -> m (T.Text, [T.Text])
languageCompletion code pos = return $ 
  let (before, _) = T.splitAt pos code
      word = last $ T.words $ T.map replace before
  in (word, map T.pack $ matches $ T.unpack word)

  where
    matches :: String -> [String]
    matches word =
      case head word of
        'p' -> ["plus"]
        'm' -> ["minus"]
        'e' -> ["exp"]
        'd' -> ["div"]
        't' -> ["times"]

    replace :: Char -> Char
    replace '(' = ' '
    replace ')' = ' '
    replace ',' = ' '
    replace  x = x

languageInspect :: Monad m => T.Text -> Int -> m (Maybe DisplayData)
languageInspect _ _ = return $ Just $ DisplayData PlainText $ T.pack $ unlines $
  [ "We support five arithmetic functions:"
  , "   - plus  +"
  , "   - minus -"
  , "   - div   /"
  , "   - times *"
  , "   - exp   ^"
  , "Expressions are written as f(exp, exp)"
  ]

simpleConfig :: KernelConfig IO String String
simpleConfig = KernelConfig
  { kernelLanguageInfo = languageConfig
  , writeKernelspec = const $ return languageKernelspec
  , displayOutput = displayString
  , displayResult = displayString
  , completion = languageCompletion
  , inspectInfo = languageInspect
  , run = languageRun
  , debug = False
  }

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["kernel", profileFile] ->
      easyKernel profileFile simpleConfig
    ["install"] -> do
      putStrLn "Installing kernelspec..."
      installKernelspec simpleConfig False Nothing
    _ -> do
      putStrLn "Usage:"
      putStrLn "fun-calc-example install      -- set up the kernelspec"
      putStrLn "fun-calc-example kernel FILE  -- run a kernel with FILE for communication with the frontend"
