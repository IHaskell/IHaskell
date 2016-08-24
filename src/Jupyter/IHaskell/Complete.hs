{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.IHaskell.Complete (Completion(..), complete) where

-- Imports from 'base'
import           Data.List (sortOn, partition, nub)
import           Data.IORef (readIORef)
import           Data.Char (isSymbol)
import           Control.Arrow ((>>>))
import           Data.Maybe (mapMaybe, isJust)
import           Control.Exception (SomeException)
import           Data.Monoid ((<>))

-- Imports from 'transformers'
import           Control.Monad.IO.Class (MonadIO(liftIO))

-- Imports from 'text'
import Data.Text (Text)
import qualified Data.Text as T

-- Imports from 'ghc'
import           GHC (moduleNameString, moduleName, getSessionDynFlags, load, guessTarget, setTargets,
                      LoadHowMuch(LoadAllTargets), InteractiveImport(IIDecl), simpleImportDecl,
                      getContext, setContext, mkModuleName, gbracket, Ghc)
import           Exception (ghandle)
import           Module (moduleEnvToList, lookupModuleEnv)
import           GhcMonad (withSession)
import           OccName (occNameString)
import           Name (nameOccName)
import           Avail (availNames)
import           PackageConfig (PackageConfig, InstalledPackageInfo(exposed), exposedModules)
import           DynFlags (DynFlags(pkgDatabase), xFlags, flagSpecName, fLangFlags, fFlags, fWarningFlags)
import           HscTypes (hsc_EPS, eps_PIT, mi_exports)

-- Imports from 'bin-package-db'
import           GHC.PackageDb (ExposedModule(exposedName))


-- Imports from 'ihaskell'
import Jupyter.IHaskell.Interpreter (Interpreter, ghc)

import Debug.Trace

data Completion = Completion { completionContext :: Int, completionText :: Text }
  deriving (Eq, Ord, Show)

-- | Generate completions at the end of a given block of text. If this function were used to
-- generate completions within an IDE, the passed in text would be all the content preceeding the
-- cursor.
complete :: Text -- ^ Text preceeding the cursor to base completions off of.
         -> Interpreter [Completion]
complete text =
  if text == ""
    then return []
    else completeLine $ T.strip $ last $ T.lines text

-- | Generate completions, assuming that the input is only one line of text. Completions are
-- generated as if the cursor is at the end of the line.
completeLine :: Text -- ^ A single line of text.
             -> Interpreter [Completion]
completeLine text
  | T.isPrefixOf ":!" text = completeFile text
  | T.isPrefixOf ":l" text = completeSourceFile text
  | T.isPrefixOf ":s" text = completeFlag text
  | T.isPrefixOf ":e" text = completeExtension text
  | T.isPrefixOf ":o" text = completeOption text
  | insideString text = completeFile text
  | T.isPrefixOf "import" text = completeImport text
  | otherwise =
      case T.splitOn "." (lastToken text) of
        [identifier] -> completeIdentifier identifier
        names        -> completeQualified names

-- | Get the last, incomplete token in the text. A token is defined as a series of contiguous
-- non-separator characters. This is left intentionally (possibly frustratingly) vague, as the
-- entire method of completion parsing is a combination of heuristics and guesswork.
lastToken :: Text -> Text
lastToken = T.takeWhileEnd (not . delimiter)
  where
    delimiter :: Char -> Bool
    delimiter char = isSymbol char || char `elem` (" -\n\t(),{}[]\\'\"`" :: String)

-- | Get the last, incomplete word in the text. A word is defined as a series of contiguous
-- non-whitespace characters.
lastWord :: Text -> Text
lastWord = snd . T.breakOnEnd " "

-- | Check whether the end of the text is ostensibly inside a string literal, handling an escaped
-- double quote properly.
--
-- >>> insideString "Hello" == False
-- >>> insideString "He \"llo" == True
-- >>> insideString "He \"ll \" o" == False
-- >>> insideString "He \"ll\\\"" == True
insideString :: Text -> Bool
insideString text =
  case T.breakOn "\"" text of
    (_, "") -> False
    ("", after) -> not $ insideString $ T.tail after
    (before, after) ->
      case T.last before of
        '\\' -> insideString $ T.tail after
        _    -> not $ insideString $ T.tail after

-- | Generate completions given an import line. There are several types of completions generated on
-- import lines:
--
--    1. Keyword completion: Autocomplete "qualified" if the user types "qu" or more.
--    2. Module name completion: Autocomplete "Data.Monoid" if the user types "Data.Mo" or more.
--    3. Module prefix completion: Autocomplete "Data." if the user types "Dat" or more.
--    4. Identifier completion: Autocomplete "mempty" if the user types "import Data.Monoid (me".
--
-- Module prefix completion should be prioritized used when there are no good module name
-- completions, where a good module name completion is one that does not require adding any periods.
completeImport :: Text -> Interpreter [Completion]
completeImport text
  | T.isPrefixOf "qu" token =
      ghc $ mkCompletions token ["qualified"]

  | otherwise =
      case T.breakOn "(" text of
        (_, "") -> completeModule (extractModuleName text)
        (before, after) ->
          case T.breakOn ")" after of
            (importList, "") -> completeIdentifierFromModule (extractModuleName before) $ lastToken importList
            _ -> return []
  where
    token = lastToken text
    extractModuleName = lastToken . T.strip

-- | Complete module names and prefixes.
--
-- (See 'completeImport' for more info on import completions.)
completeModule :: Text -- ^ The prefix of the module, e.g. @System.Env@.
               -> Interpreter [Completion]
completeModule name = do
  flags <- ghc getSessionDynFlags
  case pkgDatabase flags of
    Nothing -> return []
    Just pkgConfigs -> do
      let packageCandidates = filter (T.isPrefixOf name) . nub . concatMap getModuleNames
          candidates = packageCandidates . filter exposed $ pkgConfigs
          (good, bad) = partition goodCompletion candidates
          top =
            case good of
              [] -> sortOn T.length . nub . map mkPrefixCompletion $ bad
              _  -> sortOn T.length good
          bottom = sortOn T.length bad
      mkCompletions name (top ++ bottom)

  where
    getModuleNames :: PackageConfig -> [Text]
    getModuleNames = map (T.pack . moduleNameString . exposedName) . exposedModules

    goodCompletion :: Text -> Bool
    goodCompletion candidate = T.count "." candidate == T.count "." name

    mkPrefixCompletion :: Text -> Text
    mkPrefixCompletion =
      -- Divide a module name into the constituent directory names.
      T.splitOn "." >>>
      -- Take as many of them as there are in our query.
      take (T.count "." name + 1) >>>
      -- Put the names back together with a "." separator.
      T.intercalate "." >>>
      -- Add another separator, since we'll need one!
      flip T.append "."

-- | Complete an identifier in an import list by looking up the list of identifier in the module for
-- which this import list is for and autocompleting based on those identifiers.
completeIdentifierFromModule :: Text -- ^ Module name being imported from
                             -> Text -- ^ Prefix of the identifier being imported
                             -> Interpreter [Completion]
completeIdentifierFromModule name token = ghc $ withSession $ \env ->
  -- Ensure that we don't lose our old context; reset it after we're done.
  gbracket getContext setContext $ const $
    -- An exception will be thrown if we can't load the module; return an empty completion list.
    ghandle returnNothing $ do
      -- Ensure we have the module we're completing from loaded.
      setContext [IIDecl $ simpleImportDecl $ mkModuleName $ T.unpack name]

      -- Once the module we're completing from is loaded, all of the information should be available to
      -- us. Module interfaces are stored in the EPS (ExternalPackageState), which contains the PIT
      -- (package interface table), which contains a bunch of MIs (module interfaces). Module interfaces
      -- have a list of associated exported names, which we use for completion.
      pkgState <- liftIO $ readIORef (hsc_EPS env)
      let pkgIfaceTable = eps_PIT pkgState
          ifaces = map snd $ filter (namedModule . fst) $ moduleEnvToList pkgIfaceTable
          exports = concatMap availNames $ concatMap mi_exports ifaces
          exportNames = filter (T.isPrefixOf token) $
            map (T.pack . occNameString . nameOccName) exports
      mkCompletions token $ sortOn T.length exportNames
  where
    namedModule = (== name) . T.pack . moduleNameString . moduleName

    -- If we cannot load the module into the current context, return an empty completion list.
    returnNothing :: Monad m => SomeException -> m [a]
    returnNothing _ = return []

completeFlag :: Text -> Interpreter [Completion]
completeFlag text =
  mkCompletions token $ filter (T.isPrefixOf token) $ map T.pack allNames
  where
    token = lastWord text
    otherNames = ["-package", "-Wall", "-w"]

    -- Possibly leave out the fLangFlags? The -XUndecidableInstances vs. obsolete
    -- -fallow-undecidable-instances.
    fNames = concat [map flagSpecName fFlags, map flagSpecName fWarningFlags, map flagSpecName fLangFlags]
    fNoNames = map ("no" ++) fNames
    fAllNames = map ("-f" ++) (fNames ++ fNoNames)

    xNames = map flagSpecName xFlags
    xNoNames = map ("No" ++) xNames
    xAllNames = map ("-X" ++) (xNames ++ xNoNames)

    allNames = concat [xAllNames, otherNames, fAllNames]

completeExtension :: Text -> Interpreter [Completion]
completeExtension text =
  mkCompletions token $ filter (T.isPrefixOf token) $ xNames ++ xNoNames
  where
    token = lastWord text

    xNames = T.pack <$> map flagSpecName xFlags
    xNoNames = map ("No" <>) xNames

completeSourceFile text = return []
completeOption text = return []
completeFile text = return []
completeIdentifier identifier = return []
completeQualified names = return []

mkCompletions :: Monad m => Text -> [Text] -> m [Completion]
mkCompletions token = return . map (Completion $ T.length token)
