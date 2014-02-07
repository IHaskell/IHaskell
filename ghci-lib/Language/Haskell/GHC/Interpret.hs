module Language.Haskell.GHC.Interpret (
  -- Initialize GHC API.
  initGhci,

  -- Evaluation
  {-
  evalStatements,
  evalExpression,
  -}
  evalImport,
  {-
  evalDeclarations,
  setExtension,
  setFlag,
  getType,
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

import Data.Function (on)
import Control.Monad (void)

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
