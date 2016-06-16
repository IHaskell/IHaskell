module Main where

import qualified Data.Text as T

import           System.Environment (getArgs)

import           Text.Parsec
import           Text.Parsec.String

import           IHaskell.IPython.EasyKernel (easyKernel, installKernelspec, KernelConfig(..))
import           IHaskell.IPython.Types

-- Define the actual language!
data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Div Expr Expr
          | Exp Expr Expr
          | Val Int
  deriving (Show, Eq)

eval :: Expr -> Int
eval (Val i) = i
eval (Plus x y) = eval x + eval y
eval (Minus x y) = eval x - eval y
eval (Times x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Exp x y) = eval x ^ eval y

parseExpr :: String -> Either String Expr
parseExpr str =
  case parse expr "interactive" (filter (/= ' ') str) of
    Left err -> Left (show err)
    Right e  -> Right e
  where
    expr :: Parser Expr
    expr = val <|> op

    val :: Parser Expr
    val = Val <$> read <$> many1 (oneOf "0123456789")

    op :: Parser Expr
    op = do
      func <- choice $ map string $ ["plus", "minus", "times", "div", "exp"]
      char '('
      x <- expr
      char ','
      y <- expr
      char ')'
      return $
        case func of
          "plus"  -> Plus x y
          "minus" -> Minus x y
          "times" -> Times x y
          "div"   -> Div x y
          "exp"   -> Exp x y

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
    replace x = x

languageInspect :: Monad m => T.Text -> Int -> m (Maybe [DisplayData])
languageInspect _ _ = return $
  Just
    [ DisplayData PlainText $ T.pack $
      unlines
        [ "We support five arithmetic functions:"
        , "   - plus  +"
        , "   - minus -"
        , "   - div   /"
        , "   - times *"
        , "   - exp   ^"
        , "Expressions are written as f(exp, exp)"
        ]
    ]

languageRun :: T.Text -> IO () -> (String -> IO ()) -> IO (String, ExecuteReplyStatus, String)
languageRun code init intermediate = do
  init
  let expr = parseExpr $ T.unpack code
  intermediate (show expr)

  return
    (case expr of
       Left err   -> err
       Right expr -> show (eval expr), IHaskell.IPython.Types.Ok, "")

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
  , kernelBanner = "Simple Arithmetic Expressions"
  , kernelProtocolVersion = "5.0"
  , kernelImplName = "funcalc"
  , kernelImplVersion = "0.0"
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
      putStrLn
        "fun-calc-example kernel FILE  -- run a kernel with FILE for communication with the frontend"
