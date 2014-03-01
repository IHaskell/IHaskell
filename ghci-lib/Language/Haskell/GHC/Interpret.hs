module Language.Haskell.GHC.Interpret (
  -- Initialize GHC API.
  initGhci,

  -- Evaluation
  {-
  evalStatements,
  evalExpression,
  -}
  evalImport,
  evalDeclarations,
  setFlags,
  getType,
  {-
  loadFile,
  -}
  ) where

import InteractiveEval
import GHC
import DynFlags
import GhcMonad
import HsImpExp
import HscTypes
import RdrName
import Outputable

import Data.Function (on)
import Control.Monad (void)

import Data.String.Utils (replace)

-- | Initialize the GHC API. Run this as the first thing in the `runGhc`.
initGhci :: GhcMonad m => m ()
initGhci = do
  -- Initialize dyn flags.
  -- Start with -XExtendedDefaultRules and -XNoMonomorphismRestriction.
  originalFlags <- getSessionDynFlags
  let flag = flip xopt_set
      unflag = flip xopt_unset
      dflags = flag Opt_ExtendedDefaultRules . unflag Opt_MonomorphismRestriction $ originalFlags

  void $ setSessionDynFlags $ dflags { hscTarget = HscInterpreted,
                                       ghcLink = LinkInMemory,
                                       pprCols = 300 }

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
    importOf imp (IIDecl decl) = ((==) `on` (unLoc . ideclName)) decl imp

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

-- | Get the type of an expression.
getType :: GhcMonad m => String -> m String
getType expr = do
  result <- exprType expr
  flags <- getSessionDynFlags
  let typeStr = showSDocUnqual flags $ ppr result
  return typeStr
