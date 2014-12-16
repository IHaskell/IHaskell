{-# LANGUAGE CPP, NoImplicitPrelude #-}
module IHaskell.Eval.Util (
  -- * Initialization
  initGhci,

  -- * Flags and extensions
  -- ** Set and unset flags.
  extensionFlag, setExtension,
  ExtFlag(..),
  setFlags,

  -- * Code Evaluation
  evalImport,
  evalDeclarations,
  getType,
  getDescription,

  -- * Pretty printing
  doc,
  ) where

import ClassyPrelude

-- GHC imports.
import DynFlags
import FastString
import GHC
import GhcMonad
import HsImpExp
import HscTypes
import InteractiveEval
import Module
import Outputable
import Packages
import RdrName
import NameSet
import Name
import PprTyThing
import qualified Pretty

import Control.Monad (void)
import Data.Function (on)
import Data.String.Utils (replace)

-- | A extension flag that can be set or unset.
data ExtFlag
     = SetFlag   ExtensionFlag
     | UnsetFlag ExtensionFlag

-- | Find the extension that corresponds to a given flag. Create the
-- corresponding 'ExtFlag' via @SetFlag@ or @UnsetFlag@.
-- If no such extension exist, yield @Nothing@.
extensionFlag :: String         -- Extension name, such as @"DataKinds"@
              -> Maybe ExtFlag
extensionFlag ext =
  case find (flagMatches ext) xFlags of
    Just (_, flag, _) -> Just $ SetFlag flag
    -- If it doesn't match an extension name, try matching against
    -- disabling an extension.
    Nothing ->
      case find (flagMatchesNo ext) xFlags of
        Just (_, flag, _) -> Just $ UnsetFlag flag
        Nothing -> Nothing

  where
    -- Check if a FlagSpec matches an extension name.
    flagMatches ext (name, _, _) = ext == name

    -- Check if a FlagSpec matches "No<ExtensionName>".
    -- In that case, we disable the extension.
    flagMatchesNo ext (name, _, _) = ext == "No" ++ name

-- | Set an extension and update flags.
-- Return @Nothing@ on success. On failure, return an error message.
setExtension :: GhcMonad m => String -> m (Maybe String)
setExtension ext = do
  flags <- getSessionDynFlags
  case extensionFlag ext of
    Nothing -> return $ Just $ "Could not parse extension name: " ++ ext
    Just flag -> do
      setSessionDynFlags $
        case flag of
          SetFlag ghcFlag -> xopt_set flags ghcFlag
          UnsetFlag ghcFlag -> xopt_unset flags ghcFlag
      return Nothing

-- | Set a list of flags, as per GHCi's `:set`.
-- This was adapted from GHC's InteractiveUI.hs (newDynFlags).
-- It returns a list of error messages.
setFlags :: GhcMonad m => [String] -> m [String]
setFlags ext = do
    -- Try to parse flags.
    flags <- getSessionDynFlags
    (flags', unrecognized, warnings) <- parseDynamicFlags flags (map noLoc ext)

    -- First, try to check if this flag matches any extension name.
    let restorePkg x = x { packageFlags = packageFlags flags }
    let restoredPkgs = flags' { packageFlags = packageFlags flags}
    GHC.setProgramDynFlags restoredPkgs
    GHC.setInteractiveDynFlags restoredPkgs

    -- Create the parse errors.
    let noParseErrs = map (("Could not parse: " ++) . unLoc) unrecognized
        allWarns = map unLoc warnings ++
                     ["-package not supported yet" | packageFlags flags /= packageFlags flags']
        warnErrs    = map ("Warning: " ++) allWarns
    return $ noParseErrs ++ warnErrs

-- | Convert an 'SDoc' into a string. This is similar to the family of
-- 'showSDoc' functions, but does not impose an arbitrary width limit on
-- the output (in terms of number of columns). Instead, it respsects the
-- 'pprCols' field in the structure returned by 'getSessionDynFlags', and
-- thus gives a configurable width of output.
doc :: GhcMonad m => SDoc -> m String
doc sdoc = do
  flags <- getSessionDynFlags
  unqual <- getPrintUnqual
  let style = mkUserStyle unqual AllTheWay
  let cols = pprCols flags
      d = runSDoc sdoc (initSDocContext flags style)
  return $ Pretty.fullRender Pretty.PageMode cols 1.5 string_txt "" d
  where
    string_txt :: Pretty.TextDetails -> String -> String
    string_txt (Pretty.Chr c)   s  = c:s
    string_txt (Pretty.Str s1)  s2 = s1 ++ s2
    string_txt (Pretty.PStr s1) s2 = unpackFS s1 ++ s2
    string_txt (Pretty.LStr s1 _) s2 = unpackLitString s1 ++ s2

-- | Initialize the GHC API. Run this as the first thing in the `runGhc`.
-- This initializes some dyn flags (@ExtendedDefaultRules@,
-- @NoMonomorphismRestriction@), sets the target to interpreted, link in
-- memory, sets a reasonable output width, and potentially a few other
-- things. It should be invoked before other functions from this module.
--
-- We also require that the sandbox PackageConf (if any) is passed here
-- as setSessionDynFlags will read the package database the first time
-- (and only the first time) it is called.
initGhci :: GhcMonad m => Maybe String -> m ()
initGhci sandboxPackages = do
  -- Initialize dyn flags.
  -- Start with -XExtendedDefaultRules and -XNoMonomorphismRestriction.
  originalFlags <- getSessionDynFlags
  let flag = flip xopt_set
      unflag = flip xopt_unset
      dflags = flag Opt_ExtendedDefaultRules . unflag Opt_MonomorphismRestriction $ originalFlags
      pkgConfs = case sandboxPackages of
        Nothing -> extraPkgConfs originalFlags
        Just path ->
          let pkg  = PkgConfFile path in
            (pkg:) . extraPkgConfs originalFlags

  void $ setSessionDynFlags $ dflags { hscTarget = HscInterpreted,
                                       ghcLink = LinkInMemory,
                                       pprCols = 300,
                                       extraPkgConfs = pkgConfs }

-- | Evaluate a single import statement.
-- If this import statement is importing a module which was previously
-- imported implicitly (such as `Prelude`) or if this module has a `hiding`
-- annotation, the previous import is removed.
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
    importOf :: ImportDecl RdrName -> InteractiveImport -> Bool
    importOf _ (IIModule _) = False
    importOf imp (IIDecl decl) =
      ((==) `on` (unLoc . ideclName)) decl imp && not (ideclQualified decl)

    -- Check whether an import is an *implicit* import of something.
    implicitImportOf :: ImportDecl RdrName -> InteractiveImport -> Bool
    implicitImportOf _ (IIModule _) = False
    implicitImportOf imp (IIDecl decl) = ideclImplicit decl && imp `importOf` IIDecl decl

    -- Check whether an import is hidden.
    isHiddenImport :: ImportDecl RdrName -> Bool
    isHiddenImport imp = case ideclHiding imp of
                           Just (True, _) -> True
                           _ -> False

-- | Evaluate a series of declarations.
-- Return all names which were bound by these declarations.
evalDeclarations :: GhcMonad m => String -> m [String]
evalDeclarations decl = do
  names <- runDecls decl
  flags <- getSessionDynFlags
  return $ map (replace ":Interactive." "" . showPpr flags) names

-- | Get the type of an expression and convert it to a string.
getType :: GhcMonad m => String -> m String
getType expr = do
  result <- exprType expr
  flags <- getSessionDynFlags
  let typeStr = showSDocUnqual flags $ ppr result
  return typeStr

-- | A wrapper around @getInfo@. Return info about each name in the string.
getDescription :: GhcMonad m => String -> m [String]
getDescription str = do
  names     <- parseName str
  maybeInfos <- mapM getInfo' names

  -- Filter out types that have parents in the same set.
  -- GHCi also does this.
  let infos = catMaybes maybeInfos
      allNames = mkNameSet $ map (getName . getType) infos
      hasParent info = case tyThingParent_maybe (getType info) of
        Just parent -> getName parent `elemNameSet` allNames
        Nothing -> False
      filteredOutput = filter (not . hasParent) infos

  -- Print nicely
  mapM (doc . printInfo) filteredOutput
  where
#if MIN_VERSION_ghc(7,8,0)
    getInfo' = getInfo False
#else
    getInfo' = getInfo
#endif

#if MIN_VERSION_ghc(7,8,0)
    getType (theType, _, _, _) = theType
#else
    getType (theType, _, _) = theType
#endif

#if MIN_VERSION_ghc(7,8,0)
    printInfo (thing, fixity, classInstances, famInstances) =
          pprTyThingInContextLoc thing $$
          showFixity thing fixity $$
          vcat (map GHC.pprInstance classInstances) $$
          vcat (map GHC.pprFamInst famInstances)
#else
    printInfo (thing, fixity, classInstances) =
          pprTyThingInContextLoc False thing $$ showFixity thing fixity $$ vcat (map GHC.pprInstance classInstances)
#endif
    showFixity thing fixity =
      if fixity == GHC.defaultFixity
      then empty
      else ppr fixity <+> pprInfixName (getName thing)
