{-# LANGUAGE NoImplicitPrelude, CPP #-}

module IHaskell.Eval.Util (
    -- * Initialization
    initGhci,

    -- * Flags and extensions ** Set and unset flags.
    extensionFlag,
    setExtension,
    ExtFlag(..),
    setFlags,
    setWayDynFlag,

    -- * Code Evaluation
    evalImport,
    removeImport,
    evalDeclarations,
    getType,
    getDescription,

    -- * Pretty printing
    doc,
    pprDynFlags,
    pprLanguages,

    -- * Monad-loops
    unfoldM,
    ) where

import           IHaskellPrelude
#if MIN_VERSION_ghc(8,6,0)
#else
import qualified Data.ByteString.Char8 as CBS
#endif

-- GHC imports.
#if MIN_VERSION_ghc(9,8,0)
import           GHC.Core.InstEnv (is_cls, is_tys, mkInstEnv, instEnvElts)
import           GHC.Core.Unify
import           GHC.Data.Bag
import           GHC.Types.TyThing.Ppr
import           GHC.Driver.CmdLine
import           GHC.Driver.Monad (modifySession)
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Driver.Env.Types
import           GHC.Platform.Ways
import           GHC.Runtime.Context
import           GHC.Types.Error
import           GHC.Types.Name (pprInfixName)
import           GHC.Types.Name.Set
import           GHC.Types.TyThing
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.Utils.Error as E
import qualified GHC.Utils.Outputable as O
import qualified GHC.Utils.Ppr as Pretty
import           GHC.Runtime.Loader
#elif MIN_VERSION_ghc(9,4,0)
import           GHC.Core.InstEnv (is_cls, is_tys, mkInstEnv, instEnvElts)
import           GHC.Core.Unify
import           GHC.Types.TyThing.Ppr
import           GHC.Driver.CmdLine
import           GHC.Driver.Monad (modifySession)
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Driver.Env.Types
import           GHC.Platform.Ways
import           GHC.Runtime.Context
import           GHC.Types.Name (pprInfixName)
import           GHC.Types.Name.Set
import           GHC.Types.TyThing
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.Utils.Outputable as O
import qualified GHC.Utils.Ppr as Pretty
import           GHC.Runtime.Loader
#elif MIN_VERSION_ghc(9,2,0)
import           GHC.Core.InstEnv (is_cls, is_tys)
import           GHC.Core.Unify
import           GHC.Types.TyThing.Ppr
import           GHC.Driver.CmdLine
import           GHC.Driver.Monad (modifySession)
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Driver.Env.Types
import           GHC.Platform.Ways
import           GHC.Runtime.Context
import           GHC.Types.Name (pprInfixName)
import           GHC.Types.Name.Set
import           GHC.Types.TyThing
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.Utils.Outputable as O
import qualified GHC.Utils.Ppr as Pretty
import           GHC.Runtime.Loader
#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Core.InstEnv (is_cls, is_tys)
import           GHC.Core.Unify
import           GHC.Core.Ppr.TyThing
import           GHC.Driver.CmdLine
import           GHC.Driver.Monad (modifySession)
import           GHC.Driver.Session
import           GHC.Driver.Types
import           GHC.Driver.Ways
import           GHC.Types.Name (pprInfixName)
import           GHC.Types.Name.Set
import qualified GHC.Driver.Session as DynFlags
import qualified GHC.Utils.Outputable as O
import qualified GHC.Utils.Ppr as Pretty
import           GHC.Runtime.Loader
#else
import           DynFlags
import           GhcMonad
import           HscTypes
import           NameSet
import           Name
import           PprTyThing
import           InstEnv (ClsInst(..))
import           Unify (tcMatchTys)
import qualified Pretty
import qualified Outputable as O
#if MIN_VERSION_ghc(8,6,0)
import           DynamicLoading
#endif
#endif
#if MIN_VERSION_ghc(8,6,0)
#else
import           FastString
#endif
import           GHC

import           StringUtils (replace)

#if MIN_VERSION_ghc(9,0,0)
#elif MIN_VERSION_ghc(8,4,0)
import           CmdLineParser (warnMsg)
#endif

import           GHC.LanguageExtensions

type ExtensionFlag = Extension

-- | A extension flag that can be set or unset.
data ExtFlag = SetFlag ExtensionFlag
             | UnsetFlag ExtensionFlag

-- | Find the extension that corresponds to a given flag. Create the corresponding 'ExtFlag' via
-- @SetFlag@ or @UnsetFlag@. If no such extension exist, yield @Nothing@.
extensionFlag :: String         -- Extension name, such as @"DataKinds"@
              -> Maybe ExtFlag
extensionFlag ext =
  case find (flagMatches ext) xFlags of
    Just fs -> Just $ SetFlag $ flagSpecFlag fs
    -- If it doesn't match an extension name, try matching against disabling an extension.
    Nothing ->
      case find (flagMatchesNo ext) xFlags of
        Just fs -> Just $ UnsetFlag $ flagSpecFlag fs
        Nothing -> Nothing
  where
    -- Check if a FlagSpec matches an extension name.
    flagMatches ex fs = ex == flagSpecName fs

    -- Check if a FlagSpec matches "No<ExtensionName>". In that case, we disable the extension.
    flagMatchesNo ex fs = ex == "No" ++ flagSpecName fs

#if MIN_VERSION_ghc(9,2,0)
-- Taken from GHC
addWay' :: Way
        -> DynFlags
        -> DynFlags
addWay' w dflags0 =
  let platform = targetPlatform dflags0
      dflags1 = dflags0 { targetWays_ = addWay w (targetWays_ dflags0) }
      dflags2 = foldr setGeneralFlag' dflags1 (wayGeneralFlags platform w)
      dflags3 = foldr unSetGeneralFlag' dflags2 (wayUnsetGeneralFlags platform w)
  in dflags3
#endif

-- | Consult the RTS to find if GHC has been built with dynamic linking and then turn on the
-- dynamic way for GHC. Otherwise it does nothing.
setWayDynFlag :: DynFlags
              -> DynFlags
setWayDynFlag =
  if hostIsDynamic
  then addWay' WayDyn
  else id
#if MIN_VERSION_ghc(9,0,0)
#else
  where
    hostIsDynamic = dynamicGhc
#endif

-- | Pretty-print dynamic flags (taken from 'InteractiveUI' module of `ghc-bin`)
pprDynFlags :: Bool       -- ^ Whether to include flags which are on by default
            -> DynFlags
            -> O.SDoc
pprDynFlags show_all dflags =
  O.vcat
    [ O.text "GHCi-specific dynamic flag settings:" O.$$
      O.nest 2 (O.vcat (map (setting opt) ghciFlags))
    , O.text "other dynamic, non-language, flag settings:" O.$$
      O.nest 2 (O.vcat (map (setting opt) others))
    , O.text "warning settings:" O.$$
      O.nest 2 (O.vcat (map (setting wopt) wFlags))
    ]
  where

    wFlags = DynFlags.wWarningFlags

    opt = gopt

    setting test flag
      | quiet = O.empty :: O.SDoc
      | is_on = fstr name :: O.SDoc
      | otherwise = fnostr name :: O.SDoc
      where
        name = flagSpecName flag
        f = flagSpecFlag flag
        is_on = test f dflags
        quiet = not show_all && test f default_dflags == is_on

#if MIN_VERSION_ghc(9,6,0)
    default_dflags = defaultDynFlags (settings dflags)
#elif MIN_VERSION_ghc(8,10,0)
    default_dflags = defaultDynFlags (settings dflags) (llvmConfig dflags)
#elif MIN_VERSION_ghc(8,6,0)
    default_dflags = defaultDynFlags (settings dflags) (llvmTargets dflags, llvmPasses dflags)
#elif MIN_VERSION_ghc(8,4,0)
    default_dflags = defaultDynFlags (settings dflags) (llvmTargets dflags)
#else
    default_dflags = defaultDynFlags (settings dflags)
#endif

    fstr, fnostr :: String -> O.SDoc
    fstr str = O.text "-f" O.<> O.text str

    fnostr str = O.text "-fno-" O.<> O.text str

    (ghciFlags, others) = partition (\f -> flagSpecFlag f `elem` flgs) DynFlags.fFlags

    flgs = concat [flgs1, flgs2, flgs3]

    flgs1 = [Opt_PrintExplicitForalls]
    flgs2 = [Opt_PrintExplicitKinds]

flgs3 :: [GeneralFlag]
flgs3 = [Opt_PrintBindResult, Opt_BreakOnException, Opt_BreakOnError, Opt_PrintEvldWithShow]

-- | Pretty-print the base language and active options (taken from `InteractiveUI` module of
-- `ghc-bin`)
pprLanguages :: Bool      -- ^ Whether to include flags which are on by default
             -> DynFlags
             -> O.SDoc
pprLanguages show_all dflags =
  O.vcat
    [ O.text "base language is: " O.<>
      case language dflags of
        Nothing          -> O.text "Haskell2010"
        Just Haskell98   -> O.text "Haskell98"
        Just Haskell2010 -> O.text "Haskell2010"
#if MIN_VERSION_ghc(9,4,0)
        Just GHC2021 -> O.text "GHC2021"
#else
#endif
    , (if show_all
         then O.text "all active language options:"
         else O.text "with the following modifiers:") O.$$
      O.nest 2 (O.vcat (map (setting xopt) DynFlags.xFlags))
    ]
  where
    setting test flag
      | quiet = O.empty
      | is_on = O.text "-X" O.<> O.text name
      | otherwise = O.text "-XNo" O.<> O.text name
      where
        name = flagSpecName flag
        f = flagSpecFlag flag
        is_on = test f dflags
        quiet = not show_all && test f default_dflags == is_on

    default_dflags =
#if MIN_VERSION_ghc(9,6,0)
      defaultDynFlags (settings dflags) `lang_set`
#elif MIN_VERSION_ghc(8,10,0)
      defaultDynFlags (settings dflags) (llvmConfig dflags) `lang_set`
#elif MIN_VERSION_ghc(8,6,0)
      defaultDynFlags (settings dflags) (llvmTargets dflags, llvmPasses dflags) `lang_set`
#elif MIN_VERSION_ghc(8,4,0)
      defaultDynFlags (settings dflags) (llvmTargets dflags) `lang_set`
#else
      defaultDynFlags (settings dflags) `lang_set`
#endif
      case language dflags of
        Nothing -> Just Haskell2010
        other   -> other

-- | Set an extension and update flags. Return @Nothing@ on success. On failure, return an error
-- message.
setExtension :: GhcMonad m => String -> m (Maybe String)
setExtension ext = do
  flags <- getSessionDynFlags
  case extensionFlag ext of
    Nothing -> return $ Just $ "Could not parse extension name: " ++ ext
    Just flag -> do
      _ <- setSessionDynFlags $
        case flag of
          SetFlag ghcFlag   -> xopt_set flags ghcFlag
          UnsetFlag ghcFlag -> xopt_unset flags ghcFlag
      return Nothing

-- | Set a list of flags, as per GHCi's `:set`. This was adapted from GHC's InteractiveUI.hs
-- (newDynFlags). It returns a list of error messages.
setFlags :: GhcMonad m => [String] -> m [String]
setFlags ext = do
  -- Try to parse flags.
  flags <- getSessionDynFlags
#if MIN_VERSION_ghc(9,2,0)
  logger <- getLogger
  (flags0, unrecognized, warnings) <- parseDynamicFlags logger flags (map noLoc ext)
#else
  (flags0, unrecognized, warnings) <- parseDynamicFlags flags (map noLoc ext)
#endif

  -- We can't update packages here
  let flags1 = flags0 { packageFlags = packageFlags flags }

#if MIN_VERSION_ghc(9,2,0)
  -- Loading plugins explicitly is no longer required in 9.2
  let flags2 = flags1
#elif MIN_VERSION_ghc(8,6,0)
  -- Plugins were introduced in 8.6
  hsc_env <- GHC.getSession
  flags2 <- liftIO (initializePlugins hsc_env flags1)
#else
  let flags2 = flags1
#endif
  _ <- GHC.setProgramDynFlags flags2
  GHC.setInteractiveDynFlags flags2

  -- Create the parse errors.
  let noParseErrs = map (("Could not parse: " ++) . unLoc) unrecognized
#if MIN_VERSION_ghc(9,8,0)
      allWarns = map (show . flip O.runSDoc O.defaultSDocContext . E.formatBulleted . diagnosticMessage defaultOpts . errMsgDiagnostic) (bagToList $ getWarningMessages warnings) ++
#elif MIN_VERSION_ghc(8,4,0)
      allWarns = map (unLoc . warnMsg) warnings ++
#else
      allWarns = map unLoc warnings ++
#endif
        -- Stack appears to duplicate package flags, so we use `nub` to work around this
        ["-package not supported yet" | nub (packageFlags flags) /= nub (packageFlags flags0)]
      warnErrs = map ("Warning: " ++) allWarns
  return $ noParseErrs ++ warnErrs

-- | Convert an 'SDoc' into a string. This is similar to the family of 'showSDoc' functions, but
-- does not impose an arbitrary width limit on the output (in terms of number of columns). Instead,
-- it respsects the 'pprCols' field in the structure returned by 'getSessionDynFlags', and thus
-- gives a configurable width of output.
doc :: GhcMonad m => O.SDoc -> m String
doc sdoc = do
  flags <- getSessionDynFlags
#if MIN_VERSION_ghc(9,6,0)
  let unqual = O.neverQualify
#else
  unqual <- getPrintUnqual
#endif
#if MIN_VERSION_ghc(9,0,0)
  let style = O.mkUserStyle unqual O.AllTheWay
#elif MIN_VERSION_ghc(8,2,0)
  let style = O.mkUserStyle flags unqual O.AllTheWay
#else
  let style = O.mkUserStyle unqual O.AllTheWay
#endif
  let cols = pprCols flags
#if MIN_VERSION_ghc(9,2,0)
      d = O.runSDoc sdoc (initSDocContext flags style)
  return $ Pretty.fullRender (Pretty.PageMode False) cols 1.5 string_txt "" d
#else
      d = O.runSDoc sdoc (O.initSDocContext flags style)
  return $ Pretty.fullRender Pretty.PageMode cols 1.5 string_txt "" d
#endif

  where
    string_txt :: Pretty.TextDetails -> String -> String
#if MIN_VERSION_ghc(8,6,0)
    string_txt = Pretty.txtPrinter
#else
    string_txt (Pretty.Chr c) s = c : s
    string_txt (Pretty.Str s1) s2 = s1 ++ s2
    string_txt (Pretty.PStr s1) s2 = unpackFS s1 ++ s2
    string_txt (Pretty.LStr s1 _) s2 = unpackLitString s1 ++ s2
    string_txt (Pretty.ZStr s1) s2 = CBS.unpack (fastZStringToByteString s1) ++ s2
#endif

-- | Initialize the GHC API. Run this as the first thing in the `runGhc`. This initializes some dyn
-- flags (@ExtendedDefaultRules@,
-- @NoMonomorphismRestriction@), sets the target to interpreted, link in
-- memory, sets a reasonable output width, and potentially a few other
-- things. It should be invoked before other functions from this module.
--
-- We also require that the sandbox PackageConf (if any) is passed here
-- as setSessionDynFlags will read the package database the first time
-- (and only the first time) it is called.
initGhci :: GhcMonad m => Maybe String -> m ()
initGhci sandboxPackages = do
  -- Initialize dyn flags. Start with -XExtendedDefaultRules and -XNoMonomorphismRestriction.
#if MIN_VERSION_ghc(9,2,0)
  -- We start handling GHC environment files
  originalFlagsNoPackageEnv <- getSessionDynFlags
  logger <- getLogger
  originalFlags <- liftIO $ interpretPackageEnv logger originalFlagsNoPackageEnv
#elif MIN_VERSION_ghc(9,0,0)
  -- We start handling GHC environment files
  originalFlagsNoPackageEnv <- getSessionDynFlags
  originalFlags <- liftIO $ interpretPackageEnv originalFlagsNoPackageEnv
#else
  originalFlags <- getSessionDynFlags
#endif
  let flag = flip xopt_set
      unflag = flip xopt_unset
      dflags = flag ExtendedDefaultRules . unflag MonomorphismRestriction $ setWayDynFlag originalFlags
#if MIN_VERSION_ghc(8,2,0)
      pkgFlags =
        case sandboxPackages of
          Nothing -> packageDBFlags originalFlags
          Just path ->
#if MIN_VERSION_ghc(9,0,0)
            let pkg = PackageDB $ PkgDbPath path
#else
            let pkg = PackageDB $ PkgConfFile path
#endif
            in packageDBFlags originalFlags ++ [pkg]

  void $ setSessionDynFlags $ dflags
#if MIN_VERSION_ghc(9,6,0)
    { backend = interpreterBackend
#elif MIN_VERSION_ghc(9,2,0)
    { backend = Interpreter
#else
    { hscTarget = HscInterpreted
#endif
    , ghcLink = LinkInMemory
    , pprCols = 300
    , packageDBFlags = pkgFlags
    }
#else
      pkgConfs =
        case sandboxPackages of
          Nothing -> extraPkgConfs originalFlags
          Just path ->
            let pkg = PkgConfFile path
            in (pkg :) . extraPkgConfs originalFlags

  void $ setSessionDynFlags $ dflags
    { hscTarget = HscInterpreted
    , ghcLink = LinkInMemory
    , pprCols = 300
    , extraPkgConfs = pkgConfs
    }
#endif

-- | Evaluate a single import statement. If this import statement is importing a module which was
-- previously imported implicitly (such as `Prelude`) or if this module has a `hiding` annotation,
-- the previous import is removed.
evalImport :: GhcMonad m => String -> m ()
evalImport imports = do
  importDecl <- parseImportDecl imports
  context <- getContext

  -- If we've imported this implicitly, remove the old import.
  let noImplicit = filter (not . implicitImportOf importDecl) context

      -- If this is a `hiding` import, remove previous non-`hiding` imports.
      oldImps = if isHiddenImport importDecl
                  then filter (not . importOf importDecl) context
                  else noImplicit

  -- Replace the context.
  setContext $ IIDecl importDecl : oldImps

  where
    -- Check whether an import is the same as another import (same module).
#if MIN_VERSION_ghc(8,4,0)
    importOf :: ImportDecl GhcPs -> InteractiveImport -> Bool
#else
    importOf :: ImportDecl RdrName -> InteractiveImport -> Bool
#endif
    importOf _ (IIModule _) = False
    importOf imp (IIDecl decl) =
#if MIN_VERSION_ghc(8,10,0)
      ((==) `on` (unLoc . ideclName)) decl imp && not (isImportDeclQualified $ ideclQualified decl)
#else
      ((==) `on` (unLoc . ideclName)) decl imp && not (ideclQualified decl)
#endif

    -- Check whether an import is an *implicit* import of something.
#if MIN_VERSION_ghc(9,6,0)
    implicitImportOf :: ImportDecl GhcPs -> InteractiveImport -> Bool
    implicitImportOf _ (IIModule _) = False
    implicitImportOf imp (IIDecl decl) = ideclImplicit (ideclExt decl) && imp `importOf` IIDecl decl
#elif MIN_VERSION_ghc(8,4,0)
    implicitImportOf :: ImportDecl GhcPs -> InteractiveImport -> Bool
    implicitImportOf _ (IIModule _) = False
    implicitImportOf imp (IIDecl decl) = ideclImplicit decl && imp `importOf` IIDecl decl
#else
    implicitImportOf :: ImportDecl RdrName -> InteractiveImport -> Bool
    implicitImportOf _ (IIModule _) = False
    implicitImportOf imp (IIDecl decl) = ideclImplicit decl && imp `importOf` IIDecl decl
#endif

    -- Check whether an import is hidden.
#if MIN_VERSION_ghc(9,6,0)
    isHiddenImport :: ImportDecl GhcPs -> Bool
    isHiddenImport imp =
      case ideclImportList imp of
        Just (EverythingBut, _) -> True
        _              -> False
#elif MIN_VERSION_ghc(8,4,0)
    isHiddenImport :: ImportDecl GhcPs -> Bool
    isHiddenImport imp =
      case ideclHiding imp of
        Just (True, _) -> True
        _              -> False
#else
    isHiddenImport :: ImportDecl RdrName -> Bool
    isHiddenImport imp =
      case ideclHiding imp of
        Just (True, _) -> True
        _              -> False
#endif

removeImport :: GhcMonad m => String -> m ()
removeImport modName = do
  ctx <- getContext
  let ctx' = filter (not . (isImportOf $ mkModuleName modName)) ctx
  setContext ctx'

  where
    isImportOf :: ModuleName -> InteractiveImport -> Bool
    isImportOf name (IIModule mName) = name == mName
    isImportOf name (IIDecl impDecl) = name == unLoc (ideclName impDecl)

-- | Evaluate a series of declarations. Return all names which were bound by these declarations.
evalDeclarations :: GhcMonad m => String -> m [String]
evalDeclarations decl = do
  names <- runDecls decl
  cleanUpDuplicateInstances
  flags <- getSessionDynFlags
#if MIN_VERSION_ghc(9,2,0)
  return $ map (replace ":Interactive." "" . showPpr flags) names
#else
  return $ map (replace ":Interactive." "" . O.showPpr flags) names
#endif

cleanUpDuplicateInstances :: GhcMonad m => m ()
cleanUpDuplicateInstances = modifySession $ \hscEnv ->
  let
      -- Get all class instances
      ic = hsc_IC hscEnv
      (clsInsts, famInsts) = ic_instances ic
      -- Remove duplicates
#if MIN_VERSION_ghc(9,4,0)
      clsInsts' = mkInstEnv $ nubBy instEq $ instEnvElts clsInsts
#else
      clsInsts' = nubBy instEq clsInsts
#endif
  in hscEnv { hsc_IC = ic { ic_instances = (clsInsts', famInsts) } }
  where
    instEq :: ClsInst -> ClsInst -> Bool
    -- Only support replacing instances on GHC 7.8 and up
    instEq c1 c2 =
      is_cls c1 == is_cls c2 && isJust (tcMatchTys (is_tys c1) (is_tys c2))


-- | Get the type of an expression and convert it to a string.
getType :: GhcMonad m => String -> m String
getType expr = do
#if MIN_VERSION_ghc(8,2,0)
  result <- exprType TM_Inst expr
#else
  result <- exprType expr
#endif
  flags <- getSessionDynFlags
#if MIN_VERSION_ghc(9,2,0)
  let typeStr = showSDoc flags $ O.ppr result
#else
  let typeStr = O.showSDocUnqual flags $ O.ppr result
#endif
  return typeStr

-- | This is unfoldM from monad-loops. It repeatedly runs an IO action until it return Nothing, and
-- puts all the Justs in a list. If you find yourself using more functionality from monad-loops,
-- just add the package dependency instead of copying more code from it.
unfoldM :: IO (Maybe a) -> IO [a]
unfoldM f = maybe (return []) (\r -> (r :) <$> unfoldM f) =<< f

-- | A wrapper around @getInfo@. Return info about each name in the string.
getDescription :: GhcMonad m => String -> m [String]
getDescription str = do
  names <- parseName str
  maybeInfos <- mapM getInfo' names

  -- Filter out types that have parents in the same set. GHCi also does this.
#if MIN_VERSION_ghc(9,6,0)
  let infos = catMaybes $ nonEmptyToList maybeInfos
#else
  let infos = catMaybes maybeInfos
#endif
      allNames = mkNameSet $ map (getName . getInfoType) infos
      hasParent info =
        case tyThingParent_maybe (getInfoType info) of
          Just parent -> getName parent `elemNameSet` allNames
          Nothing     -> False
      filteredOutput = filter (not . hasParent) infos

  -- Print nicely
  mapM (doc . printInfo) filteredOutput

  where

    getInfo' = getInfo False

#if MIN_VERSION_ghc(8,4,0)
    getInfoType (theType, _, _, _, _) = theType
#else
    getInfoType (theType, _, _, _) = theType
#endif

#if MIN_VERSION_ghc(8,4,0)
    printInfo (thing, fixity, classInstances, famInstances, _) =
      pprTyThingInContextLoc thing O.$$
      showFixity thing fixity O.$$
      O.vcat (map GHC.pprInstance classInstances) O.$$
      O.vcat (map GHC.pprFamInst famInstances)
#else
    printInfo (thing, fixity, classInstances, famInstances) =
      pprTyThingInContextLoc thing O.$$
      showFixity thing fixity O.$$
      O.vcat (map GHC.pprInstance classInstances) O.$$
      O.vcat (map GHC.pprFamInst famInstances)
#endif
    showFixity thing fixity =
      if fixity == GHC.defaultFixity
        then O.empty
        else O.ppr fixity O.<+> pprInfixName (getName thing)
