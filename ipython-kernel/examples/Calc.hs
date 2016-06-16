{-# LANGUAGE FlexibleContexts, FlexibleInstances, OverloadedStrings, MultiWayIf #-}

module Main where

import           Control.Applicative
import           Control.Arrow

import           Control.Concurrent (MVar, newMVar, takeMVar, putMVar, threadDelay)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.State.Strict (StateT, get, modify, runStateT)

import           Data.Char (isDigit)
import           Data.List (isPrefixOf)
import           Data.Monoid ((<>))
import qualified Data.Text as T

import           IHaskell.IPython.Kernel
import           IHaskell.IPython.EasyKernel (installKernelspec, easyKernel, KernelConfig(..))

import           System.Environment (getArgs)
import           System.FilePath ((</>))

import           Text.Parsec (Parsec, ParseError, alphaNum, char, letter, oneOf, optionMaybe,
                              runParser, (<?>))
import qualified Text.Parsec.Token as P

import qualified Paths_ipython_kernel as Paths

---------------------------------------------------------
--  Hutton's Razor, plus time delays, plus a global state
---------------------------------------------------------
--
-- | This language is Hutton's Razor with two added operations that are needed to demonstrate the
-- kernel features: a global state, accessed and modified using Count, and a sleep operation.
data Razor = I Integer
           | Plus Razor Razor
           | SleepThen Double Razor
           | Count
  deriving (Read, Show, Eq)

-- ------- Parser -------
razorDef :: Monad m => P.GenLanguageDef String a m
razorDef = P.LanguageDef
  { P.commentStart = "(*"
  , P.commentEnd = "*)"
  , P.commentLine = "//"
  , P.nestedComments = True
  , P.identStart = letter <|> char '_'
  , P.identLetter = alphaNum <|> char '_'
  , P.opStart = oneOf "+"
  , P.opLetter = oneOf "+"
  , P.reservedNames = ["sleep", "then", "end", "count"]
  , P.reservedOpNames = []
  , P.caseSensitive = True
  }

lexer :: Monad m => P.GenTokenParser String a m
lexer = P.makeTokenParser razorDef

parens :: Parsec String a b -> Parsec String a b
parens = P.parens lexer

reserved :: String -> Parsec String a ()
reserved = P.reserved lexer

integer :: Parsec String a Integer
integer = P.integer lexer

float :: Parsec String a Double
float = P.float lexer

operator :: Parsec String a String
operator = P.operator lexer

keyword :: String -> Parsec String a ()
keyword kwd = reserved kwd <?> "the keyword \"" ++ kwd ++ "\""

literal :: Parsec String a Razor
literal = I <$> integer

sleepThen :: Parsec String a Razor
sleepThen = do
  keyword "sleep"
  delay <- float <?> "seconds"
  keyword "then"
  body <- expr
  keyword "end" <?> ""
  return $ SleepThen delay body

count :: Parsec String a Razor
count = keyword "count" >> return Count

expr :: Parsec String a Razor
expr = do
  one <- parens expr <|> literal <|> sleepThen <|> count
  rest <- optionMaybe
            (do
               op <- operator
               guard (op == "+")
               expr)
  case rest of
    Nothing    -> return one
    Just other -> return $ Plus one other

parse :: String -> Either ParseError Razor
parse = runParser expr () "(input)"

-------------------- Language operations -------------------- 
--
-- | Completion
langCompletion :: Monad m => T.Text -> Int -> m (T.Text, [T.Text])
langCompletion code pos = return $
  let (before, _) = T.splitAt pos code
  in case lastMaybe (T.words before) of
    Nothing   -> ("", [])
    Just word -> (word, map T.pack . matchesFor $ T.unpack word)
  where
    lastMaybe :: [a] -> Maybe a
    lastMaybe [] = Nothing
    lastMaybe [x] = Just x
    lastMaybe (_:xs) = lastMaybe xs
    matchesFor :: String -> [String]
    matchesFor input = filter (isPrefixOf input) available
    available = ["sleep", "then", "end", "count"] ++ map show [(-1000 :: Int) .. 1000]

-- | Documentation lookup
langInfo :: Monad m => T.Text -> Int -> m (Maybe [DisplayData])
langInfo code pos = return $ toDisplay $
  if | any (T.isPrefixOf obj) ["sleep", "then", "end"] -> Just (obj, sleepDocs, sleepType)
     | T.isPrefixOf obj "count" -> Just (obj, countDocs, countType)
     | obj == "+" -> Just (obj, plusDocs, plusType)
     | T.all isDigit obj -> Just (obj, intDocs obj, intType)
     | [x, y] <- T.splitOn "." obj
      , T.all isDigit x
      , T.all isDigit y -> Just (obj, floatDocs obj, floatType)
     | otherwise -> Nothing
  where
    (before, _) = T.splitAt pos code
    obj = last $ T.words before

    toDisplay Nothing = Nothing
    toDisplay (Just (x, y, z)) = Just [DisplayData PlainText $ T.unlines [x, y, z]]

    sleepDocs = "sleep DURATION then VALUE end: sleep DURATION seconds, then eval VALUE"
    sleepType = "sleep FLOAT then INT end"
    plusDocs = "Perform addition"
    plusType = "INT + INT"
    intDocs i = "The integer " <> i
    intType = "INT"
    floatDocs f = "The floating point value " <> f
    floatType = "FLOAT"
    countDocs = "Increment and return the current counter"
    countType = "INT"

-- | Messages sent to the frontend during evaluation will be lists of trace elements
data IntermediateEvalRes = Got Razor Integer
                         | Waiting Double
  deriving Show

-- | Cons for lists of trace elements - in this case, "sleeping" messages should replace old ones to
-- create a countdown effect.
consRes :: IntermediateEvalRes -> [IntermediateEvalRes] -> [IntermediateEvalRes]
consRes r@(Waiting _) (Waiting _:s) = r : s
consRes r s = r : s

-- | Execute an expression.
execRazor :: MVar Integer -- ^ The global counter state
          -> Razor -- ^ The term to execute
          -> IO () -- ^ Callback to clear output so far
          -> ([IntermediateEvalRes] -> IO ()) -- ^ Callback for intermediate results
          -> StateT ([IntermediateEvalRes], T.Text) IO Integer
execRazor _ x@(I i) _ _ =
  modify (second (<> T.pack (show x))) >> return i
execRazor val tm@(Plus x y) clear send =
  do
    modify (second (<> T.pack (show tm)))
    x' <- execRazor val x clear send
    modify (first $ consRes (Got x x'))
    sendState
    y' <- execRazor val y clear send
    modify (first $ consRes (Got y y'))
    sendState
    let res = x' + y'
    modify (first $ consRes (Got tm res))
    sendState
    return res

  where
    sendState = liftIO clear >> fst <$> get >>= liftIO . send
execRazor val (SleepThen delay body) clear send
  | delay <= 0.0 = execRazor val body clear send
  | delay > 0.1 = do
      modify (first $ consRes (Waiting delay))
      sendState
      liftIO $ threadDelay 100000
      execRazor val (SleepThen (delay - 0.1) body) clear send
  | otherwise = do
      modify (first $ consRes (Waiting 0))
      sendState
      liftIO $ threadDelay (floor (delay * 1000000))
      execRazor val body clear send
  where
    sendState = liftIO clear >> fst <$> get >>= liftIO . send
execRazor val Count clear send = do
  i <- liftIO $ takeMVar val
  modify (first $ consRes (Got Count i))
  sendState
  liftIO $ putMVar val (i + 1)
  return i

  where
    sendState = liftIO clear >> fst <$> get >>= liftIO . send

-- | Generate a language configuration for some initial state
mkConfig :: MVar Integer -- ^ The internal state of the execution
         -> KernelConfig IO [IntermediateEvalRes] (Either ParseError Integer)
mkConfig var = KernelConfig
  { kernelLanguageInfo = LanguageInfo
    { languageName = "expanded_huttons_razor"
    , languageVersion = "1.0.0"
    , languageFileExtension = ".txt"
    , languageCodeMirrorMode = "null"
    }
  , writeKernelspec = const $ return $ KernelSpec
    { kernelDisplayName = "Hutton's Razor"
    , kernelLanguage = "hutton"
    , kernelCommand = ["simple-calc-example", "kernel", "{connection_file}"]
    }
  , displayResult = displayRes
  , displayOutput = displayOut
  , completion = langCompletion
  , inspectInfo = langInfo
  , run = parseAndRun
  , debug = False
  , kernelBanner = "Expanded Hutton's Razor"
  , kernelProtocolVersion = "5.0"
  , kernelImplName = "expanded_huttons_razor"
  , kernelImplVersion = "0.0"
  }
  where
    displayRes (Left err) =
      [ DisplayData MimeHtml . T.pack $ "<em>" ++ show err ++ "</em>"
      , DisplayData PlainText . T.pack $ show err
      ]
    displayRes (Right x) =
      return . DisplayData MimeHtml . T.pack $
        "Answer: <strong>" ++ show x ++ "</strong>"
    displayOut out =
      let outLines = reverse (map (T.pack . show) out)
      in return (DisplayData PlainText (T.unlines outLines))
    parseAndRun code clear send =
      case parse (T.unpack code) of
        Left err -> return (Left err, Err, "")
        Right tm -> do
          (res, (_, pager)) <- runStateT (execRazor var tm clear send) ([], "")
          return (Right res, Ok, T.unpack pager)

main :: IO ()
main = do
  args <- getArgs
  val <- newMVar 1
  case args of
    ["kernel", profileFile] ->
      easyKernel profileFile (mkConfig val)
    ["install"] -> do
      putStrLn "Installing kernelspec..."
      installKernelspec (mkConfig val) False Nothing
    _ -> do
      putStrLn "Usage:"
      putStrLn "simple-calc-example install      -- set up the kernelspec"
      putStrLn
        "simple-calc-example kernel FILE  -- run a kernel with FILE for communication with the frontend"
