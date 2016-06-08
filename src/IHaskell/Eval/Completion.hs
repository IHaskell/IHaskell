{-# LANGUAGE CPP, DoAndIfThenElse, TypeFamilies, FlexibleContexts #-}

{- |
Description:    Generates tab completion options.

This has a limited amount of context sensitivity. It distinguishes between four contexts at the moment:
- import statements (completed using modules)
- identifiers (completed using in scope values)
- extensions via :ext (completed using GHC extensions)
- qualified identifiers (completed using in-scope values)

-}
module IHaskell.Eval.Completion (complete, completionTarget, completionType, CompletionType(..)) where

import           IHaskellPrelude
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Char8 as CBS

import           Control.Applicative ((<$>))
import           Data.ByteString.UTF8 hiding (drop, take, lines, length)
import           Data.Char
import           Data.List (nub, init, last, head, elemIndex)
import qualified Data.List.Split as Split
import qualified Data.List.Split.Internals as Split
import           Data.Maybe (fromJust)
import           System.Environment (getEnv)

import           GHC hiding (Qualified)
#if MIN_VERSION_ghc(7,10,0)
import           GHC.PackageDb (ExposedModule(exposedName))
#endif
import           DynFlags
import           GhcMonad
import qualified GhcMonad
import           PackageConfig
import           Outputable (showPpr)
import           MonadUtils (MonadIO)


import           System.Directory
import           System.FilePath
import           Control.Exception (try)

import           System.Console.Haskeline.Completion

import           IHaskell.Types
import           IHaskell.Eval.Evaluate (Interpreter)
import           IHaskell.Eval.ParseShell (parseShell)
import           StringUtils (replace, strip, split)

data CompletionType = Empty
                    | Identifier String
                    | DynFlag String
                    | Qualified String String
                    | ModuleName String String
                    | HsFilePath String String
                    | FilePath String String
                    | KernelOption String
                    | Extension String
  deriving (Show, Eq)
#if MIN_VERSION_ghc(7,10,0)
extName (FlagSpec { flagSpecName = name }) = name
#else
extName (name, _, _) = name

exposedName = id
#endif
complete :: String -> Int -> Interpreter (String, [String])
complete code posOffset = do
  -- Get the line of code which is being completed and offset within that line
  let findLine offset (first:rest) =
        if offset <= length first
          then (offset, first)
          else findLine (offset - length first - 1) rest
      findLine _ [] = error $ "Could not find line: " ++ show (map length $ lines code, posOffset)
      (pos, line) = findLine posOffset (lines code)


  flags <- getSessionDynFlags
  rdrNames <- map (showPpr flags) <$> getRdrNamesInScope
  scopeNames <- nub <$> map (showPpr flags) <$> getNamesInScope
  let isQualified = ('.' `elem`)
      unqualNames = nub $ filter (not . isQualified) rdrNames
      qualNames = nub $ scopeNames ++ filter isQualified rdrNames

  let Just db = pkgDatabase flags
      getNames = map (moduleNameString . exposedName) . exposedModules
      moduleNames = nub $ concatMap getNames db

  let target = completionTarget line pos
      completion = completionType line pos target

  let matchedText =
        case completion of
          HsFilePath _ match -> match
          FilePath _ match   -> match
          otherwise          -> intercalate "." target

  options <- case completion of
               Empty -> return []

               Identifier candidate ->
                 return $ filter (candidate `isPrefixOf`) unqualNames

               Qualified moduleName candidate -> do
                 trueName <- getTrueModuleName moduleName
                 let prefix = intercalate "." [moduleName, candidate]
                     completions = filter (prefix `isPrefixOf`) qualNames
                 return completions

               ModuleName previous candidate -> do
                 let prefix = if null previous
                                then candidate
                                else intercalate "." [previous, candidate]
                 return $ filter (prefix `isPrefixOf`) moduleNames

               DynFlag ext -> do
                 -- Possibly leave out the fLangFlags? The -XUndecidableInstances vs. obsolete
                 -- -fallow-undecidable-instances.
                 let kernelOptNames = concatMap getSetName kernelOpts
                     otherNames = ["-package", "-Wall", "-w"]

                     fNames = map extName fFlags ++
                              map extName fWarningFlags ++
                              map extName fLangFlags
                     fNoNames = map ("no" ++) fNames
                     fAllNames = map ("-f" ++) (fNames ++ fNoNames)

                     xNames = map extName xFlags
                     xNoNames = map ("No" ++) xNames
                     xAllNames = map ("-X" ++) (xNames ++ xNoNames)

                     allNames = xAllNames ++ otherNames ++ fAllNames

                 return $ filter (ext `isPrefixOf`) allNames

               Extension ext -> do
                 let xNames = map extName xFlags
                     xNoNames = map ("No" ++) xNames
                 return $ filter (ext `isPrefixOf`) $ xNames ++ xNoNames

               HsFilePath lineUpToCursor match -> completePathWithExtensions [".hs", ".lhs"]
                                                    lineUpToCursor

               FilePath lineUpToCursor match -> completePath lineUpToCursor

               KernelOption str -> return $
                 filter (str `isPrefixOf`) (concatMap getOptionName kernelOpts)

  return (matchedText, options)

getTrueModuleName :: String -> Interpreter String
getTrueModuleName name = do
  -- Only use the things that were actually imported
  let onlyImportDecl (IIDecl decl) = Just decl
      onlyImportDecl _ = Nothing

  -- Get all imports that we use.
  imports <- catMaybes <$> map onlyImportDecl <$> getContext

  -- Find the ones that have a qualified name attached. If this name isn't one of them, it already is
  -- the true name.
  flags <- getSessionDynFlags
  let qualifiedImports = filter (isJust . ideclAs) imports
      hasName imp = name == (showPpr flags . fromJust . ideclAs) imp
  case find hasName qualifiedImports of
    Nothing      -> return name
    Just trueImp -> return $ showPpr flags $ unLoc $ ideclName trueImp

-- | Get which type of completion this is from the surrounding context.
completionType :: String            -- ^ The line on which the completion is being done.
               -> Int                -- ^ Location of the cursor in the line.
               -> [String]          -- ^ The identifier being completed (pieces separated by dots).
               -> CompletionType
completionType line loc target
  -- File and directory completions are special
  | ":!" `isPrefixOf` stripped =
      fileComplete FilePath
  | ":l" `isPrefixOf` stripped =
      fileComplete HsFilePath

  -- Complete :set, :opt, and :ext
  | ":s" `isPrefixOf` stripped =
      DynFlag candidate
  | ":o" `isPrefixOf` stripped =
      KernelOption candidate
  | ":e" `isPrefixOf` stripped =
      Extension candidate

  -- Use target for other completions. If it's empty, no completion.
  | null target =
      Empty

  -- When in a string, complete filenames.
  | cursorInString line loc =
      FilePath (getStringTarget lineUpToCursor) (getStringTarget lineUpToCursor)

  -- Complete module names in imports and elsewhere.
  | "import" `isPrefixOf` stripped && isModName =
      ModuleName dotted candidate
  | isModName && (not . null . init) target =
      Qualified dotted candidate

  -- Default to completing identifiers.
  | otherwise =
      Identifier candidate
  where
    stripped = strip line
    dotted = dots target
    candidate
      | null target = ""
      | otherwise = last target
    dots = intercalate "." . init
    isModName = all isCapitalized (init target)

    isCapitalized [] = False
    isCapitalized (x:_) = isUpper x

    lineUpToCursor = take loc line
    fileComplete filePath =
      case parseShell lineUpToCursor of
        Right xs -> filePath lineUpToCursor $
          if last xs `isSuffixOf` lineUpToCursor
            then last xs
            else []
        Left _ -> Empty

    cursorInString str loc = nquotes (take loc str) `mod` 2 /= 0

    nquotes ('\\':'"':xs) = nquotes xs
    nquotes ('"':xs) = 1 + nquotes xs
    nquotes (_:xs) = nquotes xs
    nquotes [] = 0

    -- Get the bit of a string that might be a filename completion. Logic is a bit convoluted, but
    -- basically go backwards from the end, stopping at any quote or space, unless they are escaped.
    getStringTarget :: String -> String
    getStringTarget = go "" . reverse
      where
        go acc rest =
          case rest of
            '"':'\\':rem -> go ('"' : acc) rem
            '"':rem      -> acc
            ' ':'\\':rem -> go (' ' : acc) rem
            ' ':rem      -> acc
            x:rem        -> go (x : acc) rem
            []           -> acc

-- | Get the word under a given cursor location.
completionTarget :: String -> Int -> [String]
completionTarget code cursor = expandCompletionPiece pieceToComplete
  where
    pieceToComplete = map fst <$> find (elem cursor . map snd) pieces
    pieces = splitAlongCursor $ Split.split splitter $ zip code [1 ..]
    splitter = Split.defaultSplitter
      { 
      -- Split using only the characters, which are the first elements of the (char, index) tuple
      Split.delimiter = Split.Delimiter [uncurry isDelim]
      -- Condense multiple delimiters into one and then drop them.
      , Split.condensePolicy = Split.Condense
      , Split.delimPolicy = Split.Drop
      }

    isDelim :: Char -> Int -> Bool
    isDelim char idx = char `elem` neverIdent || isSymbol char

    splitAlongCursor :: [[(Char, Int)]] -> [[(Char, Int)]]
    splitAlongCursor [] = []
    splitAlongCursor (x:xs) =
      case elemIndex cursor $ map snd x of
        Nothing  -> x : splitAlongCursor xs
        Just idx -> take (idx + 1) x : drop (idx + 1) x : splitAlongCursor xs

    -- These are never part of an identifier.
    neverIdent :: String
    neverIdent = " \n\t(),{}[]\\'\"`"

    expandCompletionPiece Nothing = []
    expandCompletionPiece (Just str) = Split.splitOn "." str

getHome :: IO String
getHome = do
  homeEither <- try $ getEnv "HOME" :: IO (Either SomeException String)
  return $
    case homeEither of
      Left _     -> "~"
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
  where
    acceptAll = const True

completePathWithExtensions :: [String] -> String -> Interpreter [String]
completePathWithExtensions extensions line =
  completePathFilter (extensionIsOneOf extensions) acceptAll line ""
  where
    acceptAll = const True
    extensionIsOneOf exts str = any correctEnding exts
      where
        correctEnding ext = ext `isSuffixOf` str

completePathFilter :: (String -> Bool)      -- ^ File filter: test whether to include this file.
                   -> (String -> Bool)      -- ^ Directory filter: test whether to include this directory.
                   -> String               -- ^ Line contents to the left of the cursor.
                   -> String               -- ^ Line contents to the right of the cursor.
                   -> Interpreter [String]
completePathFilter includeFile includeDirectory left right = GhcMonad.liftIO $ do
  -- Get the completions from Haskeline.  It has a bit of a strange API.
  expanded <- dirExpand left
  completions <- map replacement <$> snd <$> completeFilename (reverse expanded, right)

  -- Split up into files and directories. Filter out ones we don't want.
  areDirs <- mapM doesDirectoryExist completions
  let dirs = filter includeDirectory $ map fst $ filter snd $ zip completions areDirs
      files = filter includeFile $ map fst $ filter (not . snd) $ zip completions areDirs

  -- Return directories before files. However, stick everything that starts with a dot after
  -- everything else. If we wanted to keep original order, we could instead use
  --   filter (`elem` (dirs ++ files)) completions
  suggestions <- mapM unDirExpand $ dirs ++ files
  let isHidden str = isPrefixOf "." . last . split "/" $
        if "/" `isSuffixOf` str
          then init str
          else str
      visible = filter (not . isHidden) suggestions
      hidden = filter isHidden suggestions
  return $ visible ++ hidden
