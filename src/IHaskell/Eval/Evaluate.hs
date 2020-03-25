{-# LANGUAGE NoOverloadedStrings, NoImplicitPrelude, TypeSynonymInstances, GADTs, CPP #-}

{- | Description : Wrapper around GHC API, exposing a single `evaluate` interface that runs
                   a statement, declaration, import, or directive.

This module exports all functions used for evaluation of IHaskell input.
-}
module IHaskell.Eval.Evaluate (
    interpret,
    testInterpret,
    testEvaluate,
    evaluate,
    flushWidgetMessages,
    Interpreter,
    liftIO,
    typeCleaner,
    formatType,
    capturedIO,
    ) where

import           IHaskellPrelude

import           Control.Concurrent (forkIO, threadDelay)
import           Data.Foldable (foldMap)
import           Prelude (head, tail, last, init)
import           Data.List (nubBy)
import qualified Data.Set as Set
import           Data.Char as Char
import           Data.Dynamic
import qualified Data.Serialize as Serialize
import qualified Debugger
import           System.Directory
import           System.Posix.IO (fdToHandle)
import           System.IO (hGetChar, hSetEncoding, utf8)
import           System.Random (getStdGen, randomRs)
import           System.Process
import           System.Exit
import           Data.Maybe (mapMaybe)
import           System.Environment (getEnv)

import qualified GHC.Paths
import           InteractiveEval
import           DynFlags
import           Exception (gtry)
import           HscTypes
import           GhcMonad (liftIO)
import           GHC hiding (Stmt, TypeSig)
import           Exception hiding (evaluate)
import           Outputable hiding ((<>))
import           Packages
import           Bag
import qualified ErrUtils

import           IHaskell.Types
import           IHaskell.IPython
import           IHaskell.Eval.Parser
import           IHaskell.Eval.Lint
import           IHaskell.Display
import qualified IHaskell.Eval.Hoogle as Hoogle
import           IHaskell.Eval.Util
import           IHaskell.BrokenPackages
import           StringUtils (replace, split, strip, rstrip)

#if MIN_VERSION_ghc(8,2,0)
import           FastString (unpackFS)
#else
import           Paths_ihaskell (version)
import           Data.Version (versionBranch)
#endif



-- | Set GHC's verbosity for debugging
ghcVerbosity :: Maybe Int
ghcVerbosity = Nothing -- Just 5

ignoreTypePrefixes :: [String]
ignoreTypePrefixes = [ "GHC.Types"
                     , "GHC.Base"
                     , "GHC.Show"
                     , "System.IO"
                     , "GHC.Float"
                     , ":Interactive"
                     , "GHC.Num"
                     , "GHC.IO"
                     , "GHC.Integer.Type"
                     ]

typeCleaner :: String -> String
typeCleaner = useStringType . foldl' (.) id (map (`replace` "") fullPrefixes)
  where
    fullPrefixes = map (++ ".") ignoreTypePrefixes
    useStringType = replace "[Char]" "String"

-- MonadIO constraint necessary for GHC 7.6
write :: (MonadIO m, GhcMonad m) => KernelState -> String -> m ()
write state x = when (kernelDebug state) $ liftIO $ hPutStrLn stderr $ "DEBUG: " ++ x

type Interpreter = Ghc

requiredGlobalImports :: [String]
requiredGlobalImports =
  [ "import qualified Prelude as IHaskellPrelude"
  , "import qualified System.Directory as IHaskellDirectory"
  , "import qualified System.Posix.IO as IHaskellIO"
  , "import qualified System.IO as IHaskellSysIO"
  , "import qualified Language.Haskell.TH as IHaskellTH"
  ]

ihaskellGlobalImports :: [String]
ihaskellGlobalImports =
  [ "import IHaskell.Display()"
  , "import qualified IHaskell.Display"
  , "import qualified IHaskell.IPython.Stdin"
  , "import qualified IHaskell.Eval.Widgets"
  ]

hiddenPackageNames :: Set.Set String
hiddenPackageNames = Set.fromList ["ghc-lib", "ghc-lib-parser"]

-- | Interpreting function for testing.
testInterpret :: Interpreter a -> IO a
testInterpret v = interpret GHC.Paths.libdir False False (const v)

-- | Evaluation function for testing.
testEvaluate :: String -> IO ()
testEvaluate str = void $ testInterpret $
  evaluate defaultKernelState str (\_ _ -> return ()) (\state _ -> return state)

-- | Run an interpreting action. This is effectively runGhc with initialization
-- and importing. The `allowedStdin` argument indicates whether `stdin` is
-- handled specially, which cannot be done in a testing environment. The
-- `needsSupportLibraries` argument indicates whether we want support libraries
-- to be imported, which is not the case during testing. The argument passed to
-- the action indicates whether the IHaskell library is available.
interpret :: String -> Bool -> Bool -> (Bool -> Interpreter a) -> IO a
interpret libdir allowedStdin needsSupportLibraries action = runGhc (Just libdir) $ do
  -- If we're in a sandbox, add the relevant package database
  sandboxPackages <- liftIO getSandboxPackageConf
  initGhci sandboxPackages
  case ghcVerbosity of
    Just verb -> do
      dflags <- getSessionDynFlags
      void $ setSessionDynFlags $ dflags { verbosity = verb }
    Nothing -> return ()

  hasSupportLibraries <- initializeImports needsSupportLibraries

  -- Close stdin so it can't be used. Otherwise it'll block the kernel forever.
  dir <- liftIO getIHaskellDir
  let cmd = printf "IHaskell.IPython.Stdin.fixStdin \"%s\"" dir
  when (allowedStdin && hasSupportLibraries) $ void $
    execStmt cmd execOptions

  initializeItVariable

  -- Run the rest of the interpreter
  action hasSupportLibraries

packageIdString' :: DynFlags -> PackageConfig -> String
packageIdString' dflags pkg_cfg =
#if MIN_VERSION_ghc(8,2,0)
    case (lookupPackage dflags $ packageConfigId pkg_cfg) of
      Nothing -> "(unknown)"
      Just cfg -> let
        PackageName name = packageName cfg
        in unpackFS name
#else
    fromMaybe "(unknown)" (unitIdPackageIdString dflags $ packageConfigId pkg_cfg)
#endif

getPackageConfigs :: DynFlags -> [PackageConfig]
getPackageConfigs dflags =
    foldMap snd pkgDb
  where
    Just pkgDb = pkgDatabase dflags

-- | Initialize our GHC session with imports and a value for 'it'. Return whether the IHaskell
-- library is available.
initializeImports :: Bool -> Interpreter Bool
initializeImports importSupportLibraries = do
  -- Load packages that start with ihaskell-*, aren't just IHaskell, and depend directly on the right
  -- version of the ihaskell library. Also verify that the packages we load are not broken.
  dflags <- getSessionDynFlags
  broken <- liftIO getBrokenPackages
  (dflgs, _) <- liftIO $ initPackages dflags
  let db = getPackageConfigs dflgs
      packageNames = map (packageIdString' dflgs) db
      hiddenPackages = Set.intersection hiddenPackageNames (Set.fromList packageNames)
      hiddenFlags = fmap HidePackage $ Set.toList hiddenPackages
      initStr = "ihaskell-"

#if MIN_VERSION_ghc(8,2,0)
      -- Name of the ihaskell package, i.e. "ihaskell"
      iHaskellPkgName = "ihaskell"
#else
      -- Name of the ihaskell package, e.g. "ihaskell-1.2.3.4"
      iHaskellPkgName = initStr ++ intercalate "." (map show (versionBranch version))
#endif

      displayPkgs = [ pkgName
                    | pkgName <- packageNames
                    , Just (x:_) <- [stripPrefix initStr pkgName]
                    , pkgName `notElem` broken
                    , isAlpha x ]

      hasIHaskellPackage = not $ null $ filter (== iHaskellPkgName) packageNames

  -- Generate import statements all Display modules.
  let capitalize :: String -> String
      capitalize [] = []
      capitalize (first:rest) = Char.toUpper first : rest

      importFmt = "import IHaskell.Display.%s"


#if MIN_VERSION_ghc(8,2,0)
      toImportStmt :: String -> String
      toImportStmt = printf importFmt . concatMap capitalize . drop 1 . split "-"
#else
      dropFirstAndLast :: [a] -> [a]
      dropFirstAndLast = reverse . drop 1 . reverse . drop 1

      toImportStmt :: String -> String
      toImportStmt = printf importFmt . concatMap capitalize . dropFirstAndLast . split "-"
#endif

      displayImports = map toImportStmt displayPkgs

  void $ setSessionDynFlags $ dflgs { packageFlags = hiddenFlags ++ packageFlags dflgs }

  -- Import implicit prelude.
  importDecl <- parseImportDecl "import Prelude"
  let implicitPrelude = importDecl { ideclImplicit = True }
      displayImports' = if importSupportLibraries then displayImports else []

  -- Import modules.
  imports <- mapM parseImportDecl $ requiredGlobalImports ++ if hasIHaskellPackage
                                                               then ihaskellGlobalImports ++ displayImports'
                                                               else []
  setContext $ map IIDecl $ implicitPrelude : imports

  return hasIHaskellPackage

-- | Give a value for the `it` variable.
initializeItVariable :: Interpreter ()
initializeItVariable =
  -- This is required due to the way we handle `it` in the wrapper statements - if it doesn't exist,
  -- the first statement will fail.
  void $ execStmt "let it = ()" execOptions

-- | Publisher for IHaskell outputs. The first argument indicates whether this output is final
-- (true) or intermediate (false). The second argument indicates whether the evaluation
-- completed successfully (Success) or an error occurred (Failure).
type Publisher = (EvaluationResult -> ErrorOccurred -> IO ())

-- | Output of a command evaluation.
data EvalOut =
       EvalOut
         { evalStatus :: ErrorOccurred
         , evalResult :: Display
         , evalState :: KernelState
         , evalPager :: [DisplayData]
         , evalMsgs :: [WidgetMsg]
         }

cleanString :: String -> String
cleanString istr = if allBrackets
                  then clean
                  else istr
  where
    str = strip istr
    l = lines str
    allBrackets = all (fAny [isPrefixOf ">", null]) l
    fAny fs x = any ($ x) fs
    clean = unlines $ map removeBracket l
    removeBracket ('>':xs) = xs
    removeBracket [] = []
    -- should never happen:
    removeBracket other = error $ "Expected bracket as first char, but got string: " ++ other

-- | Evaluate some IPython input code.
evaluate :: KernelState                  -- ^ The kernel state.
         -> String                       -- ^ Haskell code or other interpreter commands.
         -> Publisher                    -- ^ Function used to publish data outputs.
         -> (KernelState -> [WidgetMsg] -> IO KernelState) -- ^ Function to handle widget messages
         -> Interpreter KernelState
evaluate kernelState code output widgetHandler = do
  cmds <- parseString (cleanString code)
  let execCount = getExecutionCounter kernelState

  -- Extract all parse errors.
  let justError x@ParseError{} = Just x
      justError _ = Nothing
      errs = mapMaybe (justError . unloc) cmds

  updated <- case errs of
               -- Only run things if there are no parse errors.
               [] -> do
                 when (getLintStatus kernelState /= LintOff) $ liftIO $ do
                   lintSuggestions <- lint code cmds
                   unless (noResults lintSuggestions) $
                     output (FinalResult lintSuggestions [] []) Success

                 runUntilFailure kernelState (map unloc cmds ++ [storeItCommand execCount])
               -- Print all parse errors.
               _ -> do
                 forM_ errs $ \err -> do
                   out <- evalCommand output err kernelState
                   liftIO $ output
                     (FinalResult (evalResult out) [] [])
                     (evalStatus out)
                 return kernelState

  return updated { getExecutionCounter = execCount + 1 }

  where
    noResults (Display res) = null res
    noResults (ManyDisplay res) = all noResults res

    runUntilFailure :: KernelState -> [CodeBlock] -> Interpreter KernelState
    runUntilFailure state [] = return state
    runUntilFailure state (cmd:rest) = do
      evalOut <- evalCommand output cmd state

      -- Get displayed channel outputs. Merge them with normal display outputs.
      dispsMay <- if supportLibrariesAvailable state
                    then do
                      getEncodedDisplays <- extractValue "IHaskell.Display.displayFromChanEncoded"
                      case getEncodedDisplays of
                        Left err -> error $ "Deserialization error (Evaluate.hs): " ++ err
                        Right displaysIO -> do
                          result <- liftIO displaysIO
                          case Serialize.decode result of
                            Left err  -> error $ "Deserialization error (Evaluate.hs): " ++ err
                            Right res -> return res
                    else return Nothing
      let result =
            case dispsMay of
              Nothing    -> evalResult evalOut
              Just disps -> evalResult evalOut <> disps

      -- Output things only if they are non-empty.
      unless (noResults result && null (evalPager evalOut)) $
        liftIO $ output
          (FinalResult result (evalPager evalOut) [])
          (evalStatus evalOut)

      let tempMsgs = evalMsgs evalOut
          tempState = evalState evalOut { evalMsgs = [] }

      -- Handle the widget messages
      newState <- if supportLibrariesAvailable state
                    then flushWidgetMessages tempState tempMsgs widgetHandler
                    else return tempState

      case evalStatus evalOut of
        Success -> runUntilFailure newState rest
        Failure -> return newState

    storeItCommand execCount = Statement $ printf "let it%d = it" execCount

-- | Compile a string and extract a value from it. Effectively extract the result of an expression
-- from inside the notebook environment.
extractValue :: Typeable a => String -> Interpreter (Either String a)
extractValue expr = do
  compiled <- dynCompileExpr expr
  case fromDynamic compiled of
    Nothing     -> return (Left multipleIHaskells)
    Just result -> return (Right result)

  where
    multipleIHaskells =
      concat
        [ "The installed IHaskell support libraries do not match"
        , " the instance of IHaskell you are running.\n"
        , "This *may* cause problems with functioning of widgets or rich media displays.\n"
        , "This is most often caused by multiple copies of IHaskell"
        , " being installed simultaneously in your environment.\n"
        , "To resolve this issue, clear out your environment and reinstall IHaskell.\n"
        , "If you are installing support libraries, make sure you only do so once:\n"
        , "    # Run this without first running `stack install ihaskell`\n"
        , "    stack install ihaskell-diagrams\n"
        , "If you continue to have problems, please file an issue on Github."
        ]

flushWidgetMessages :: KernelState
                    -> [WidgetMsg]
                    -> (KernelState -> [WidgetMsg] -> IO KernelState)
                    -> Interpreter KernelState
flushWidgetMessages state evalmsgs widgetHandler = do
  -- Capture all widget messages queued during code execution
  extracted <- extractValue "IHaskell.Eval.Widgets.relayWidgetMessages"
  liftIO $
    case extracted of
      Left err -> do
        hPutStrLn stderr "Disabling IHaskell widget support due to an encountered error:"
        hPutStrLn stderr err
        return state
      Right messagesIO -> do
        messages <- messagesIO

        -- Handle all the widget messages
        let commMessages = evalmsgs ++ messages
        widgetHandler state commMessages


getErrMsgDoc :: ErrUtils.ErrMsg -> SDoc
getErrMsgDoc = ErrUtils.pprLocErrMsg

safely :: KernelState -> Interpreter EvalOut -> Interpreter EvalOut
safely state = ghandle handler . ghandle sourceErrorHandler
  where
    handler :: SomeException -> Interpreter EvalOut
    handler exception =
      return
        EvalOut
          { evalStatus = Failure
          , evalResult = displayError $ show exception
          , evalState = state
          , evalPager = []
          , evalMsgs = []
          }

    sourceErrorHandler :: SourceError -> Interpreter EvalOut
    sourceErrorHandler srcerr = do
      let msgs = bagToList $ srcErrorMessages srcerr
      errStrs <- forM msgs $ doc . getErrMsgDoc

      let fullErr = unlines errStrs

      return
        EvalOut
          { evalStatus = Failure
          , evalResult = displayError fullErr
          , evalState = state
          , evalPager = []
          , evalMsgs = []
          }

wrapExecution :: KernelState
              -> Interpreter Display
              -> Interpreter EvalOut
wrapExecution state exec = safely state $
  exec >>= \res ->
    return
      EvalOut
        { evalStatus = Success
        , evalResult = res
        , evalState = state
        , evalPager = []
        , evalMsgs = []
        }

-- | Return the display data for this command, as well as whether it resulted in an error.
evalCommand :: Publisher -> CodeBlock -> KernelState -> Interpreter EvalOut
evalCommand _ (Import importStr) state = wrapExecution state $ do
  write state $ "Import: " ++ importStr
  evalImport importStr
  return mempty

evalCommand _ (Module contents) state = wrapExecution state $ do
  write state $ "Module:\n" ++ contents

  -- Write the module contents to a temporary file in our work directory
  namePieces <- getModuleName contents
  let directory = "./" ++ intercalate "/" (init namePieces) ++ "/"
      filename = last namePieces ++ ".hs"
  liftIO $ do
    createDirectoryIfMissing True directory
    writeFile (directory ++ filename) contents

  -- Clear old modules of this name
  let modName = intercalate "." namePieces
  removeTarget $ TargetModule $ mkModuleName modName
  removeTarget $ TargetFile filename Nothing

  -- Remember which modules we've loaded before.
  importedModules <- getContext

  let
      -- Get the dot-delimited pieces of the module name.
      moduleNameOf :: InteractiveImport -> [String]
      moduleNameOf (IIDecl decl) = split "." . moduleNameString . unLoc . ideclName $ decl
      moduleNameOf (IIModule imp) = split "." . moduleNameString $ imp

      -- Return whether this module prevents the loading of the one we're trying to load. If a module B
      -- exist, we cannot load A.B. All modules must have unique last names (where A.B has last name B).
      -- However, we *can* just reload a module.
      preventsLoading md =
        let pieces = moduleNameOf md
        in last namePieces == last pieces && namePieces /= pieces

  -- If we've loaded anything with the same last name, we can't use this. Otherwise, GHC tries to load
  -- the original *.hs fails and then fails.
  case find preventsLoading importedModules of
    -- If something prevents loading this module, return an error.
    Just previous -> do
      let prevLoaded = intercalate "." (moduleNameOf previous)
      return $ displayError $
        printf "Can't load module %s because already loaded %s" modName prevLoaded

    -- Since nothing prevents loading the module, compile and load it.
    Nothing -> doLoadModule modName modName

-- | Directives set via `:set`.
evalCommand _output (Directive SetDynFlag flagsStr) state = safely state $ do
  write state $ "All Flags: " ++ flagsStr

  -- Find which flags are IHaskell flags, and which are GHC flags
  let flags = words flagsStr

      -- Get the kernel state updater for any IHaskell flag; Nothing for things that aren't IHaskell
      -- flags.
      ihaskellFlagUpdater :: String -> Maybe (KernelState -> KernelState)
      ihaskellFlagUpdater flag = getUpdateKernelState <$> find (elem flag . getSetName) kernelOpts

      (ihaskellFlags, ghcFlags) = partition (isJust . ihaskellFlagUpdater) flags

  write state $ "IHaskell Flags: " ++ unwords ihaskellFlags
  write state $ "GHC Flags: " ++ unwords ghcFlags

  if null flags
    then do
      flgs <- getSessionDynFlags
      return
        EvalOut
          { evalStatus = Success
          , evalResult = Display
                           [ plain $ showSDoc flgs $ vcat
                                                        [ pprDynFlags False flgs
                                                        , pprLanguages False flgs
                                                        ]
                           ]
          , evalState = state
          , evalPager = []
          , evalMsgs = []
          }
    else do
      -- Apply all IHaskell flag updaters to the state to get the new state
      let state' = foldl' (.) id (mapMaybe ihaskellFlagUpdater ihaskellFlags) state
      errs <- setFlags ghcFlags
      let disp =
            case errs of
              [] -> mempty
              _  -> displayError $ intercalate "\n" errs

      -- For -XNoImplicitPrelude, remove the Prelude import. For -XImplicitPrelude, add it back in.
      if "-XNoImplicitPrelude" `elem` flags
        then evalImport "import qualified Prelude as Prelude"
        else when ("-XImplicitPrelude" `elem` flags) $ do
          importDecl <- parseImportDecl "import Prelude"
          let implicitPrelude = importDecl { ideclImplicit = True }
          imports <- getContext
          setContext $ IIDecl implicitPrelude : imports

      return
        EvalOut
          { evalStatus = Success
          , evalResult = disp
          , evalState = state'
          , evalPager = []
          , evalMsgs = []
          }

evalCommand output (Directive SetExtension opts) state = do
  write state $ "Extension: " ++ opts
  let set = concatMap (" -X" ++) $ words opts
  evalCommand output (Directive SetDynFlag set) state

evalCommand _output (Directive LoadModule mods) state = wrapExecution state $ do
  write state $ "Load Module: " ++ mods
  let stripped@(firstChar:remainder) = mods
      (modules, removeModule) =
        case firstChar of
          '+' -> (words remainder, False)
          '-' -> (words remainder, True)
          _   -> (words stripped, False)

  forM_ modules $ \modl -> if removeModule
                             then removeImport modl
                             else evalImport $ "import " ++ modl

  return mempty

evalCommand _output (Directive SetOption opts) state = do
  write state $ "Option: " ++ opts
  let nonExisting = filter (not . optionExists) $ words opts
  if not $ null nonExisting
    then let err = "No such options: " ++ intercalate ", " nonExisting
         in return
              EvalOut
                { evalStatus = Failure
                , evalResult = displayError err
                , evalState = state
                , evalPager = []
                , evalMsgs = []
                }
    else let options = mapMaybe findOption $ words opts
             updater = foldl' (.) id $ map getUpdateKernelState options
         in return
              EvalOut
                { evalStatus = Success
                , evalResult = mempty
                , evalState = updater state
                , evalPager = []
                , evalMsgs = []
                }

  where
    optionExists = isJust . findOption
    findOption opt =
      find (elem opt . getOptionName) kernelOpts

evalCommand _ (Directive GetType expr) state = wrapExecution state $ do
  write state $ "Type: " ++ expr
  formatType <$> ((expr ++ " :: ") ++) <$> getType expr

evalCommand _ (Directive GetKind expr) state = wrapExecution state $ do
  write state $ "Kind: " ++ expr
  (_, kind) <- GHC.typeKind False expr
  flags <- getSessionDynFlags
  let typeStr = showSDocUnqual flags $ ppr kind
  return $ formatType $ expr ++ " :: " ++ typeStr

evalCommand _ (Directive LoadFile names) state = wrapExecution state $ do
  write state $ "Load: " ++ names

  displays <- forM (words names) $ \name -> do
                let filename = if ".hs" `isSuffixOf` name
                                 then name
                                 else name ++ ".hs"
                contents <- liftIO $ readFile filename
                modName <- intercalate "." <$> getModuleName contents
                doLoadModule filename modName
  return (ManyDisplay displays)

evalCommand publish (Directive ShellCmd cmd) state = wrapExecution state $
  -- Assume the first character of 'cmd' is '!'.
  case words $ drop 1 cmd of
    "cd":dirs -> do
      -- Get home so we can replace '~` with it.
      homeEither <- liftIO (try $ getEnv "HOME" :: IO (Either SomeException String))
      let home =
            case homeEither of
              Left _  -> "~"
              Right v -> v

      let directory = replace "~" home $ unwords dirs
      exists <- liftIO $ doesDirectoryExist directory
      if exists
        then do
          -- Set the directory in IHaskell native code, for future shell commands. This doesn't set it for
          -- user code, though.
          liftIO $ setCurrentDirectory directory

          -- Set the directory for user code.
          let cmd1 = printf "IHaskellDirectory.setCurrentDirectory \"%s\"" $
                replace " " "\\ " $
                  replace "\"" "\\\"" directory
          _ <- execStmt cmd1 execOptions
          return mempty
        else return $ displayError $ printf "No such directory: '%s'" directory
    cmd1 -> liftIO $ do
      (pipe, hdl) <- createPipe
      let initProcSpec = shell $ unwords cmd1
          procSpec = initProcSpec
            { std_in = Inherit
            , std_out = UseHandle hdl
            , std_err = UseHandle hdl
            }
      (_, _, _, process) <- createProcess procSpec

      -- Accumulate output from the process.
      outputAccum <- liftIO $ newMVar ""

      -- Start a loop to publish intermediate results.
      let
          -- Compute how long to wait between reading pieces of the output. `threadDelay` takes an
          -- argument of microseconds.
          ms = 1000
          delay = 100 * ms

          -- Maximum size of the output (after which we truncate).
          maxSize = 100 * 1000
          incSize = 200
          output str = publish $ IntermediateResult $ Display [plain str]

          loop = do
            -- Wait and then check if the computation is done.
            threadDelay delay

            -- Read next chunk and append to accumulator.
            nextChunk <- readChars pipe "\n" incSize
            modifyMVar_ outputAccum (return . (++ nextChunk))

            -- Check if we're done.
            mExitCode <- getProcessExitCode process
            case mExitCode of
              Nothing -> do
                -- Write to frontend and repeat.
                readMVar outputAccum >>= flip output Success
                loop
              Just exitCode -> do
                next <- readChars pipe "" maxSize
                modifyMVar_ outputAccum (return . (++ next))
                out <- readMVar outputAccum
                case exitCode of
                  ExitSuccess -> return $ Display [plain out]
                  ExitFailure code -> do
                    let errMsg = "Process exited with error code " ++ show code
                        htmlErr = printf "<span class='err-msg'>%s</span>" errMsg
                    return $ Display
                               [ plain $ out ++ "\n" ++ errMsg
                               , html $ printf "<span class='mono'>%s</span>" out ++ htmlErr
                               ]

      loop
-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive GetHelp _) state = do
  write state "Help via :help or :?."
  return
    EvalOut
      { evalStatus = Success
      , evalResult = Display [out]
      , evalState = state
      , evalPager = []
      , evalMsgs = []
      }

  where
    out = plain $ intercalate "\n"
                    [ "The following commands are available:"
                    , "    :extension <Extension>    -  Enable a GHC extension."
                    , "    :extension No<Extension>  -  Disable a GHC extension."
                    , "    :type <expression>        -  Print expression type."
                    , "    :info <name>              -  Print all info for a name."
                    , "    :hoogle <query>           -  Search for a query on Hoogle."
                    , "    :doc <ident>              -  Get documentation for an identifier via Hoogle."
                    , "    :set -XFlag -Wall         -  Set an option (like ghci)."
                    , "    :option <opt>             -  Set an option."
                    , "    :option no-<opt>          -  Unset an option."
                    , "    :?, :help                 -  Show this help text."
                    , "    :sprint <value>           -  Print a value without forcing evaluation."
                    , ""
                    , "Any prefix of the commands will also suffice, e.g. use :ty for :type."
                    , ""
                    , "Options:"
                    , "  lint        – enable or disable linting."
                    , "  svg         – use svg output (cannot be resized)."
                    , "  show-types  – show types of all bound names"
                    , "  show-errors – display Show instance missing errors normally."
                    , "  pager       – use the pager to display results of :info, :doc, :hoogle, etc."
                    ]

-- This is taken largely from GHCi's info section in InteractiveUI.
evalCommand _ (Directive GetInfo str) state = safely state $ do
  write state $ "Info: " ++ str
  -- Get all the info for all the names we're given.
  strings <- unlines <$> getDescription str

  -- Make pager work without html by porting to newer architecture
  let htmlify str1 =
        html $
          concat
            [ "<div style='background: rgb(247, 247, 247);'><form><textarea id='code'>"
            , str1
            , "</textarea></form></div>"
            , "<script>CodeMirror.fromTextArea(document.getElementById('code'),"
            , " {mode: 'haskell', readOnly: 'nocursor'});</script>"
            ]

  return
    EvalOut
      { evalStatus = Success
      , evalResult = mempty
      , evalState = state
      , evalPager = [plain strings, htmlify strings]
      , evalMsgs = []
      }

evalCommand _ (Directive SearchHoogle query) state = safely state $ do
  results <- liftIO $ Hoogle.search query
  return $ hoogleResults state results

evalCommand _ (Directive GetDoc query) state = safely state $ do
  results <- liftIO $ Hoogle.document query
  return $ hoogleResults state results

evalCommand _ (Directive SPrint binding) state = wrapExecution state $ do
  flags <- getSessionDynFlags
  contents <- liftIO $ newIORef []
  let action = \_dflags _sev _srcspan _ppr _style msg -> modifyIORef' contents (showSDoc flags msg :)
  let flags' = flags { log_action = action }
  _ <- setSessionDynFlags flags'
  Debugger.pprintClosureCommand False False binding
  _ <- setSessionDynFlags flags
  sprint <- liftIO $ readIORef contents
  return $ formatType (unlines sprint)

evalCommand output (Statement stmt) state = wrapExecution state $ evalStatementOrIO output state
                                                                    (CapturedStmt stmt)

evalCommand output (Expression expr) state = do
  write state $ "Expression:\n" ++ expr

  -- Try to use `display` to convert our type into the output Dislay If typechecking fails and there
  -- is no appropriate typeclass instance, this will throw an exception and thus `attempt` will return
  -- False, and we just resort to plaintext.
  let displayExpr = printf "(IHaskell.Display.display (%s))" expr :: String
#if MIN_VERSION_ghc(8,2,0)
  canRunDisplay <- attempt $ exprType TM_Inst displayExpr
#else
  canRunDisplay <- attempt $ exprType displayExpr
#endif

  -- Check if this is a widget.
  let widgetExpr = printf "(IHaskell.Display.Widget (%s))" expr :: String
#if MIN_VERSION_ghc(8,2,0)
  isWidget <- attempt $ exprType TM_Inst widgetExpr
#else
  isWidget <- attempt $ exprType widgetExpr
#endif

  -- Check if this is a template haskell declaration
  let declExpr = printf "((id :: IHaskellTH.DecsQ -> IHaskellTH.DecsQ) (%s))" expr :: String
  let anyExpr = printf "((id :: IHaskellPrelude.Int -> IHaskellPrelude.Int) (%s))" expr :: String
#if MIN_VERSION_ghc(8,2,0)
  isTHDeclaration <- liftM2 (&&) (attempt $ exprType TM_Inst declExpr) (not <$> attempt (exprType TM_Inst anyExpr))
#else
  isTHDeclaration <- liftM2 (&&) (attempt $ exprType declExpr) (not <$> attempt (exprType anyExpr))
#endif

  write state $ "Can Display: " ++ show canRunDisplay
  write state $ "Is Widget: " ++ show isWidget
  write state $ "Is Declaration: " ++ show isTHDeclaration

  if isTHDeclaration
    then
    -- If it typechecks as a DecsQ, we do not want to display the DecsQ, we just want the
    -- declaration made.
    do
      _ <- write state "Suppressing display for template haskell declaration"
      _ <- GHC.runDecls expr
      return
        EvalOut
          { evalStatus = Success
          , evalResult = mempty
          , evalState = state
          , evalPager = []
          , evalMsgs = []
          }
    else if canRunDisplay
           then
           -- Use the display. As a result, `it` is set to the output.
           useDisplay displayExpr
           else do
             -- Evaluate this expression as though it's just a statement. The output is bound to 'it', so we can
             -- then use it.
             evalOut <- evalCommand output (Statement expr) state

             let out = evalResult evalOut
                 showErr = isShowError out

             -- If evaluation failed, return the failure. If it was successful, we may be able to use the
             -- IHaskellDisplay typeclass.
             return $ if not showErr || useShowErrors state
                        then evalOut
                        else postprocessShowError evalOut

  where
    -- Try to evaluate an action. Return True if it succeeds and False if it throws an exception. The
    -- result of the action is discarded.
    attempt :: Interpreter a -> Interpreter Bool
    attempt action = gcatch (action >> return True) failure
      where
        failure :: SomeException -> Interpreter Bool
        failure _ = return False

    -- Check if the error is due to trying to print something that doesn't implement the Show typeclass.
    isShowError (ManyDisplay _) = False
    isShowError (Display errs) =
      -- Note that we rely on this error message being 'type cleaned', so that `Show` is not displayed as
      -- GHC.Show.Show. This is also very fragile!
      "No instance for (Show" `isPrefixOf` msg &&
      isInfixOf "print it" msg
      where
        msg = extractPlain errs

    isSvg (DisplayData mime _) = mime == MimeSvg

    removeSvg :: Display -> Display
    removeSvg (Display disps) = Display $ filter (not . isSvg) disps
    removeSvg (ManyDisplay disps) = ManyDisplay $ map removeSvg disps

    useDisplay _displayExpr = do
      -- If there are instance matches, convert the object into a Display. We also serialize it into a
      -- bytestring. We get the bytestring IO action as a dynamic and then convert back to a bytestring,
      -- which we promptly unserialize. Note that attempting to do this without the serialization to
      -- binary and back gives very strange errors - all the types match but it refuses to decode back
      -- into a Display. Suppress output, so as not to mess up console. First, evaluate the expression in
      -- such a way that we have access to `it`.
      io <- isIO expr
      let stmtTemplate = if io
                           then "it <- (%s)"
                           else "let { it = %s }"
      evalOut <- evalCommand output (Statement $ printf stmtTemplate expr) state
      case evalStatus evalOut of
        Failure -> return evalOut
        Success -> wrapExecution state $ do
          -- Compile the display data into a bytestring.
          let cexpr = "fmap IHaskell.Display.serializeDisplay (IHaskell.Display.display it)"
          displayedBytestring <- dynCompileExpr cexpr

          -- Convert from the bytestring into a display.
          case fromDynamic displayedBytestring of
            Nothing -> error "Expecting lazy Bytestring"
            Just bytestringIO -> do
              bytestring <- liftIO bytestringIO
              case Serialize.decode bytestring of
                Left err -> error err
                Right disp ->
                  return $
                    if useSvg state
                      then disp :: Display
                      else removeSvg disp

#if MIN_VERSION_ghc(8,2,0)
    isIO exp = attempt $ exprType TM_Inst $ printf "((\\x -> x) :: IO a -> IO a) (%s)" exp
#else
    isIO exp = attempt $ exprType $ printf "((\\x -> x) :: IO a -> IO a) (%s)" exp
#endif

    postprocessShowError :: EvalOut -> EvalOut
    postprocessShowError evalOut = evalOut { evalResult = Display $ map postprocess disps }
      where
        Display disps = evalResult evalOut
        txt = extractPlain disps

        postprocess (DisplayData MimeHtml _) =
          html $ printf fmt unshowableType
                    (formatErrorWithClass "err-msg collapse" txt) script
          where
            fmt = "<div class='collapse-group'><span class='btn btn-default' href='#' id='unshowable'>Unshowable:<span class='show-type'>%s</span></span>%s</div><script>%s</script>"
            script = unlines
                       [ "$('#unshowable').on('click', function(e) {"
                       , "    e.preventDefault();"
                       , "    var $this = $(this);"
                       , "    var $collapse = $this.closest('.collapse-group').find('.err-msg');"
                       , "    $collapse.collapse('toggle');"
                       , "});"
                       ]

        postprocess other = other

        unshowableType = fromMaybe "" $ do
          let pieces = words txt
              before = takeWhile (/= "arising") pieces
              after = init $ unwords $ tail $ dropWhile (/= "(Show") before

          firstChar <- headMay after
          return $ if firstChar == '('
                     then init $ tail after
                     else after


evalCommand _ (Declaration decl) state = wrapExecution state $ do
  write state $ "Declaration:\n" ++ decl
  boundNames <- evalDeclarations decl
  let nonDataNames = filter (not . isUpper . head) boundNames

  -- Display the types of all bound names if the option is on. This is similar to GHCi :set +t.
  if not $ useShowTypes state
    then return mempty
    else do
      -- Get all the type strings.
      dflags <- getSessionDynFlags
      types <- forM nonDataNames $ \name -> do
#if MIN_VERSION_ghc(8,2,0)
                 theType <- showSDocUnqual dflags . ppr <$> exprType TM_Inst name
#else
                 theType <- showSDocUnqual dflags . ppr <$> exprType name
#endif
                 return $ name ++ " :: " ++ theType

      return $ Display [html $ unlines $ map formatGetType types]

evalCommand _ (TypeSignature sig) state = wrapExecution state $
  -- We purposefully treat this as a "success" because that way execution continues. Empty type
  -- signatures are likely due to a parse error later on, and we want that to be displayed.
  return $ displayError $ "The type signature " ++ sig ++ "\nlacks an accompanying binding."

evalCommand _ (ParseError loc err) state = do
  write state "Parse Error."
  return
    EvalOut
      { evalStatus = Failure
      , evalResult = displayError $ formatParseError loc err
      , evalState = state
      , evalPager = []
      , evalMsgs = []
      }

evalCommand _ (Pragma (PragmaUnsupported pragmaType) _pragmas) state = wrapExecution state $
  return $ displayError $ "Pragmas of type " ++ pragmaType ++ "\nare not supported."

evalCommand output (Pragma PragmaLanguage pragmas) state = do
  write state $ "Got LANGUAGE pragma " ++ show pragmas
  evalCommand output (Directive SetExtension $ unwords pragmas) state

hoogleResults :: KernelState -> [Hoogle.HoogleResult] -> EvalOut
hoogleResults state results =
  EvalOut
    { evalStatus = Success
    , evalResult = mempty
    , evalState = state
    , evalPager = [ plain $ unlines $ map (Hoogle.render Hoogle.Plain) results
                  , html $ unlines $ map (Hoogle.render Hoogle.HTML) results
                  ]
    , evalMsgs = []
    }

doLoadModule :: String -> String -> Ghc Display
doLoadModule name modName = do
  -- Remember which modules we've loaded before.
  importedModules <- getContext

  flip gcatch (unload importedModules) $ do
    -- Compile loaded modules.
    flags <- getSessionDynFlags
    errRef <- liftIO $ newIORef []
    _ <- setSessionDynFlags $ flip gopt_set Opt_BuildDynamicToo
      flags
        { hscTarget = objTarget flags
        , log_action = \_dflags _sev _srcspan _ppr _style msg -> modifyIORef' errRef (showSDoc flags msg :)
        }

    -- Load the new target.
    target <- guessTarget name Nothing
    oldTargets <- getTargets
    -- Add a target, but make sure targets are unique!
    addTarget target
    getTargets >>= return . nubBy ((==) `on` targetId) >>= setTargets
    result <- load LoadAllTargets

    -- Reset the context, since loading things screws it up.
    initializeItVariable

    -- Reset targets if we failed.
    case result of
      Failed      -> setTargets oldTargets
      Succeeded{} -> return ()

    -- Add imports
    setContext $
      case result of
        Failed    -> importedModules
        Succeeded -> IIDecl (simpleImportDecl $ mkModuleName modName) : importedModules

    -- Switch back to interpreted mode.
    _ <- setSessionDynFlags flags

    case result of
      Succeeded -> return mempty
      Failed -> do
        errorStrs <- unlines <$> reverse <$> liftIO (readIORef errRef)
        return $ displayError $ "Failed to load module " ++ modName ++ "\n" ++ errorStrs

  where
    unload :: [InteractiveImport] -> SomeException -> Ghc Display
    unload imported exception = do
      print $ show exception
      -- Explicitly clear targets
      setTargets []
      _ <- load LoadAllTargets

      -- Switch to interpreted mode!
      flags <- getSessionDynFlags
      _ <- setSessionDynFlags flags { hscTarget = HscInterpreted }

      -- Return to old context, make sure we have `it`.
      setContext imported
      initializeItVariable

      return $ displayError $ "Failed to load module " ++ modName ++ ": " ++ show exception

objTarget :: DynFlags -> HscTarget
#if MIN_VERSION_ghc(8,10,0)
objTarget = defaultObjectTarget
#else
objTarget flags = defaultObjectTarget $ targetPlatform flags
#endif

data Captured a = CapturedStmt String
                | CapturedIO (IO a)

capturedEval :: (String -> IO ()) -- ^ Function used to publish intermediate output.
             -> Captured a -- ^ Statement to evaluate.
             -> Interpreter (String, ExecResult) -- ^ Return the output and result.
capturedEval output stmt = do
  -- Generate random variable names to use so that we cannot accidentally override the variables by
  -- using the right names in the terminal.
  gen <- liftIO getStdGen
  let
      -- Variable names generation.
      rand = take 20 $ randomRs ('0', '9') gen
      var name = name ++ rand

      -- Variables for the pipe input and outputs.
      readVariable = var "file_read_var_"
      writeVariable = var "file_write_var_"

      -- Variable where to store old stdout.
      oldVariableStdout = var "old_var_stdout_"

      -- Variable where to store old stderr.
      oldVariableStderr = var "old_var_stderr_"

      -- Variable used to store true `it` value.
      itVariable = var "it_var_"

      voidpf str = printf $ str ++ " IHaskellPrelude.>> IHaskellPrelude.return ()"

      -- Statements run before the thing we're evaluating.
      initStmts =
        [ printf "let %s = it" itVariable
        , printf "(%s, %s) <- IHaskellIO.createPipe" readVariable writeVariable
        , printf "%s <- IHaskellIO.dup IHaskellIO.stdOutput" oldVariableStdout
        , printf "%s <- IHaskellIO.dup IHaskellIO.stdError" oldVariableStderr
        , voidpf "IHaskellIO.dupTo %s IHaskellIO.stdOutput" writeVariable
        , voidpf "IHaskellIO.dupTo %s IHaskellIO.stdError" writeVariable
        , voidpf "IHaskellSysIO.hSetBuffering IHaskellSysIO.stdout IHaskellSysIO.NoBuffering"
        , voidpf "IHaskellSysIO.hSetBuffering IHaskellSysIO.stderr IHaskellSysIO.NoBuffering"
        , printf "let it = %s" itVariable
        ]

      -- Statements run after evaluation.
      postStmts =
        [ printf "let %s = it" itVariable
        , voidpf "IHaskellSysIO.hFlush IHaskellSysIO.stdout"
        , voidpf "IHaskellSysIO.hFlush IHaskellSysIO.stderr"
        , voidpf "IHaskellIO.dupTo %s IHaskellIO.stdOutput" oldVariableStdout
        , voidpf "IHaskellIO.dupTo %s IHaskellIO.stdError" oldVariableStderr
        , voidpf "IHaskellIO.closeFd %s" writeVariable
        , printf "let it = %s" itVariable
        ]

      goStmt :: String -> Ghc ExecResult
      goStmt s = execStmt s execOptions

      runWithResult (CapturedStmt str) = goStmt str
      runWithResult (CapturedIO io) = do
        stat <- gcatch (liftIO io >> return NoException) (return . AnyException)
        return $
          case stat of
            NoException    -> ExecComplete (Right []) 0
            AnyException e -> ExecComplete (Left e)   0

  -- Initialize evaluation context.
  forM_ initStmts goStmt

  -- This works fine on GHC 8.0 and newer
  dyn <- dynCompileExpr readVariable
  pipe <- case fromDynamic dyn of
            Nothing -> error "Evaluate: Bad pipe"
            Just fd -> liftIO $ do
                hdl <- fdToHandle fd
                hSetEncoding hdl utf8
                return hdl

  -- Keep track of whether execution has completed.
  completed <- liftIO $ newMVar False
  finishedReading <- liftIO newEmptyMVar
  outputAccum <- liftIO $ newMVar ""

  -- Start a loop to publish intermediate results.
  let
      -- Compute how long to wait between reading pieces of the output. `threadDelay` takes an
      -- argument of microseconds.
      ms = 1000
      delay = 100 * ms

      -- Maximum size of the output (after which we truncate).
      maxSize = 100 * 1000

      loop = do
        -- Wait and then check if the computation is done.
        threadDelay delay
        computationDone <- readMVar completed

        if not computationDone
          then do
            -- Read next chunk and append to accumulator.
            nextChunk <- readChars pipe "\n" 100
            modifyMVar_ outputAccum (return . (++ nextChunk))

            -- Write to frontend and repeat.
            readMVar outputAccum >>= output
            loop
          else do
            -- Read remainder of output and accumulate it.
            nextChunk <- readChars pipe "" maxSize
            modifyMVar_ outputAccum (return . (++ nextChunk))

            -- We're done reading.
            putMVar finishedReading True

  _ <- liftIO $ forkIO loop

  result <- gfinally (runWithResult stmt) $ do
              -- Execution is done.
              liftIO $ modifyMVar_ completed (const $ return True)

              -- Finalize evaluation context.
              void $ forM postStmts goStmt

              -- Once context is finalized, reading can finish. Wait for reading to finish to that the output
              -- accumulator is completely filled.
              liftIO $ takeMVar finishedReading

  printedOutput <- liftIO $ readMVar outputAccum
  return (printedOutput, result)

data AnyException = NoException
                  | AnyException SomeException

capturedIO :: Publisher -> KernelState -> IO a -> Interpreter Display
capturedIO publish state action = do
  let showError = return . displayError . show
      handler e@SomeException{} = showError e
  gcatch (evalStatementOrIO publish state (CapturedIO action)) handler

-- | Evaluate a @Captured@, and then publish the final result to the frontend. Returns the final
-- Display.
evalStatementOrIO :: Publisher -> KernelState -> Captured a -> Interpreter Display
evalStatementOrIO publish state cmd = do
  let output str = publish . IntermediateResult $ Display [plain str]

  case cmd of
    CapturedStmt stmt ->
      write state $ "Statement:\n" ++ stmt
    CapturedIO _ ->
      write state "Evaluating Action"

  (printed, result) <- capturedEval (flip output Success) cmd
  case result of
    ExecComplete (Right names) _ -> do
      dflags <- getSessionDynFlags

      let allNames = map (showPpr dflags) names
          isItName name =
            name == "it" ||
            name == "it" ++ show (getExecutionCounter state)
          nonItNames = filter (not . isItName) allNames
          oput = [ plain printed
                   | not . null $ strip printed ]

      write state $ "Names: " ++ show allNames

      -- Display the types of all bound names if the option is on. This is similar to GHCi :set +t.
      if not $ useShowTypes state
        then return $ Display oput
        else do
          -- Get all the type strings.
          types <- forM nonItNames $ \name -> do
#if MIN_VERSION_ghc(8,2,0)
                     theType <- showSDocUnqual dflags . ppr <$> exprType TM_Inst name
#else
                     theType <- showSDocUnqual dflags . ppr <$> exprType name
#endif
                     return $ name ++ " :: " ++ theType

          let joined = unlines types
              htmled = unlines $ map formatGetType types

          return $
            case extractPlain oput of
              "" -> Display [html htmled]

              -- Return plain and html versions. Previously there was only a plain version.
              txt -> Display [plain $ joined ++ "\n" ++ txt, html $ htmled ++ mono txt]

    ExecComplete (Left exception) _ -> throw exception
    ExecBreak{} -> error "Should not break."

-- Read from a file handle until we hit a delimiter or until we've read as many characters as
-- requested
readChars :: Handle -> String -> Int -> IO String
readChars _handle _delims 0 =
  -- If we're done reading, return nothing.
  return []
readChars hdl delims nchars = do
  -- Try reading a single character. It will throw an exception if the handle is already closed.
  tryRead <- gtry $ hGetChar hdl :: IO (Either SomeException Char)
  case tryRead of
    Right ch ->
      -- If this is a delimiter, stop reading.
      if ch `elem` delims
        then return [ch]
        else do
          next <- readChars hdl delims (nchars - 1)
          return $ ch : next
    -- An error occurs at the end of the stream, so just stop reading.
    Left _ -> return []

formatError :: ErrMsg -> String
formatError = formatErrorWithClass "err-msg"

formatErrorWithClass :: String -> ErrMsg -> String
formatErrorWithClass cls =
  printf "<span class='%s'>%s</span>" cls .
  replace "\n" "<br/>" .
  fixDollarSigns .
  replace "<" "&lt;" .
  replace ">" "&gt;" .
  replace "&" "&amp;" .
  replace useDashV "" .
  replace "Ghci" "IHaskell" .
  replace "‘interactive:" "‘" .
  rstrip .
  typeCleaner
  where
    fixDollarSigns = replace "$" "<span>&dollar;</span>"
    useDashV = "\n    Use -v to see a list of the files searched for."

formatParseError :: StringLoc -> String -> ErrMsg
formatParseError (Loc ln col) =
  printf "Parse error (line %d, column %d): %s" ln col

formatGetType :: String -> String
formatGetType = printf "<span class='get-type'>%s</span>"

formatType :: String -> Display
formatType typeStr = Display [plain typeStr, html $ formatGetType typeStr]

displayError :: ErrMsg -> Display
displayError msg = Display [plain . typeCleaner $ msg, html $ formatError msg]

mono :: String -> String
mono = printf "<span class='mono'>%s</span>"
