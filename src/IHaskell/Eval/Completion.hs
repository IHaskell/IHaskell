{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, DoAndIfThenElse #-}
{- |
Description : Generates tab completion options.

This has a limited amount of context sensitivity. It distinguishes between four contexts at the moment:
- import statements (completed using modules)
- identifiers (completed using in scope values)
- extensions via :ext (completed using GHC extensions)
- qualified identifiers (completed using in-scope values)

-}
module IHaskell.Eval.Completion (complete, completionTarget, completionType, CompletionType(..)) where

import ClassyPrelude hiding (liftIO)
--import Prelude

import Control.Applicative ((<$>))
import Data.ByteString.UTF8 hiding (drop, take)
import Data.Char
import Data.List (nub, init, last, head, elemIndex)
import Data.List.Split
import Data.List.Split.Internals
import Data.Maybe
import Data.String.Utils (strip, startswith, endswith, replace)
import qualified Data.String.Utils as StringUtils
import System.Environment (getEnv)

import GHC
import DynFlags
import GhcMonad
import PackageConfig
import Outputable (showPpr)


import System.Directory
import System.FilePath
import MonadUtils (MonadIO)

import System.Console.Haskeline.Completion

import IHaskell.Types
import IHaskell.Eval.Evaluate (Interpreter)
import IHaskell.Eval.ParseShell (parseShell)


data CompletionType
     = Empty
     | Identifier String
     | Extension String
     | Qualified String String
     | ModuleName String String
     | HsFilePath String String
     | FilePath   String String 
     deriving (Show, Eq)

complete :: String -> Int -> Interpreter (String, [String])
complete line pos = do
  flags <- getSessionDynFlags
  rdrNames <- map (showPpr flags) <$> getRdrNamesInScope
  scopeNames <- nub <$> map (showPpr flags) <$> getNamesInScope
  let isQualified = ('.' `elem`)
      unqualNames = nub $ filter (not . isQualified) rdrNames
      qualNames = nub $ scopeNames ++ filter isQualified rdrNames

  let Just db = pkgDatabase flags
      getNames = map moduleNameString . exposedModules
      moduleNames = nub $ concatMap getNames db

  let target = completionTarget line pos

  let matchedText = case completionType line pos target of 
        HsFilePath _ match ->  match
        FilePath   _ match ->  match
        otherwise       -> intercalate "." target

  options <-
        case completionType line pos target of
          Empty -> return []

          Identifier candidate ->
            return $ filter (candidate `isPrefixOf`) unqualNames

          Qualified moduleName candidate -> do
            trueName <- getTrueModuleName moduleName
            let prefix = intercalate "." [trueName, candidate]
                completions = filter (prefix `isPrefixOf`)  qualNames
                falsifyName = replace trueName moduleName
            return $ map falsifyName completions

          ModuleName previous candidate -> do
            let prefix = if null previous
                         then candidate
                         else intercalate "." [previous, candidate]
            return $ filter (prefix `isPrefixOf`) moduleNames

          Extension ext -> do
            let extName (name, _, _) = name
                names = map extName xFlags
                nonames = map ("No" ++) names
            return $ filter (ext `isPrefixOf`) $ names ++ nonames

          HsFilePath lineUpToCursor match -> completePathWithExtensions [".hs", ".lhs"] lineUpToCursor

          FilePath   lineUpToCursor match  -> completePath lineUpToCursor

  return (matchedText, options)

getTrueModuleName :: String -> Interpreter String
getTrueModuleName name = do
  -- Only use the things that were actually imported
  let onlyImportDecl (IIDecl decl) = Just decl
      onlyImportDecl _ = Nothing

  -- Get all imports that we use.
  imports <- catMaybes <$> map onlyImportDecl <$> getContext

  -- Find the ones that have a qualified name attached.
  -- If this name isn't one of them, it already is the true name.
  flags <- getSessionDynFlags
  let qualifiedImports = filter (isJust . ideclAs) imports
      hasName imp = name == (showPpr flags . fromJust . ideclAs) imp
  case find hasName qualifiedImports of
    Nothing -> return name
    Just trueImp -> return $ showPpr flags $ unLoc $ ideclName trueImp

-- | Get which type of completion this is from the surrounding context.
completionType :: String            -- ^ The line on which the completion is being done.
              -> Int                -- ^ Location of the cursor in the line.
               -> [String]          -- ^ The identifier being completed (pieces separated by dots).
               -> CompletionType
completionType line loc target
  -- File and directory completions are special
  | startswith ":!" stripped
    = case parseShell lineUpToCursor of 
          Right xs -> FilePath lineUpToCursor $ if endswith (last xs) lineUpToCursor then (last xs) else []
          Left  _  -> Empty
  | startswith ":l" stripped
    = case parseShell lineUpToCursor of 
          Right xs -> HsFilePath lineUpToCursor $ if endswith (last xs) lineUpToCursor then (last xs) else []
          Left  _  -> Empty
  -- Use target for other completions.
  -- If it's empty, no completion.
  | null target
    = Empty
  | startswith "import" stripped && isModName
    = ModuleName dotted candidate
  | isModName && (not . null . init) target
    = Qualified dotted candidate
  | startswith ":e" stripped
    = Extension candidate
  | otherwise
    = Identifier candidate
  where stripped = strip line
        dotted = dots target
        candidate = last target
        dots = intercalate "." . init
        isModName = all isCapitalized (init target)
        isCapitalized = isUpper . head
        lineUpToCursor = take loc line


-- | Get the word under a given cursor location.
completionTarget :: String -> Int -> [String]
completionTarget code cursor = expandCompletionPiece pieceToComplete
  where
    pieceToComplete = map fst <$> find (elem cursor . map snd) pieces
    pieces = splitAlongCursor $ split splitter $ zip code [1 .. ]
    splitter = defaultSplitter {
      -- Split using only the characters, which are the first elements of
      -- the (char, index) tuple
      delimiter = Delimiter [uncurry isDelim],
      -- Condense multiple delimiters into one and then drop them.
      condensePolicy = Condense,
      delimPolicy = Drop
    }

    isDelim :: Char -> Int -> Bool 
    isDelim char idx = char `elem` neverIdent  || isSymbol char

    splitAlongCursor :: [[(Char, Int)]] -> [[(Char, Int)]]
    splitAlongCursor [] = []
    splitAlongCursor (x:xs) =
      case elemIndex cursor $  map snd x of
        Nothing -> x:splitAlongCursor xs
        Just idx -> take (idx + 1) x:drop (idx + 1) x:splitAlongCursor xs

    -- These are never part of an identifier.
    neverIdent :: String
    neverIdent = " \n\t(),{}[]\\'\"`"

    expandCompletionPiece Nothing = []
    expandCompletionPiece (Just str) = splitOn "." str

getHome :: IO String
getHome = do
  homeEither <- try  $ getEnv "HOME" :: IO (Either SomeException String)
  return $ case homeEither of
    Left _ -> "~"
    Right home -> home

dirExpand :: String -> IO String
dirExpand str = do
  home <- getHome
  return $ replace "~" home str

unDirExpand :: String -> IO String
unDirExpand str = do
  home <- getHome
  return $ replace home "~" str

completePath :: String -> Interpreter [String]
completePath line = completePathFilter acceptAll acceptAll line ""
  where acceptAll = const True

completePathWithExtensions :: [String] -> String -> Interpreter [String]
completePathWithExtensions extensions line =
  completePathFilter (extensionIsOneOf extensions) acceptAll line ""
  where
    acceptAll = const True
    extensionIsOneOf exts str = any correctEnding exts
      where correctEnding ext = endswith ext str

completePathFilter :: (String -> Bool)      -- ^ File filter: test whether to include this file.
                   -> (String -> Bool)      -- ^ Directory filter: test whether to include this directory.
                   -> String               -- ^ Line contents to the left of the cursor.
                   -> String               -- ^ Line contents to the right of the cursor.
                   -> Interpreter [String]
completePathFilter includeFile includeDirectory left right = liftIO $ do
  -- Get the completions from Haskeline.  It has a bit of a strange API.
  expanded <- dirExpand left
  completions <- map replacement <$> snd <$> completeFilename (reverse expanded, right)

  -- Split up into files and directories.
  -- Filter out ones we don't want.
  areDirs <- mapM doesDirectoryExist completions
  let dirs  = filter includeDirectory $ map fst $ filter snd         $ zip completions areDirs
      files = filter includeFile      $ map fst $ filter (not . snd) $ zip completions areDirs

  -- Return directories before files. However, stick everything that starts
  -- with a dot after everything else.  If we wanted to keep original
  -- order, we could instead use
  --   filter (`elem` (dirs ++ files)) completions
  suggestions <- mapM unDirExpand $ dirs ++ files
  let isHidden str = startswith "." . last . StringUtils.split "/" $
        if endswith "/" str
        then init str
        else str
      visible = filter (not . isHidden) suggestions
      hidden  = filter isHidden suggestions
  return $ visible ++ hidden
  
