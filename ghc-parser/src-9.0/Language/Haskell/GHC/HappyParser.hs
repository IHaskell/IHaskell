module Language.Haskell.GHC.HappyParser
    ( fullStatement
    , fullImport
    , fullDeclaration
    , fullExpression
    , fullTypeSignature
    , fullModule
    ) where

import GHC.Parser
import GHC.Types.SrcLoc

-- compiler/hsSyn
import GHC.Hs

-- compiler/utils
import GHC.Data.OrdList

-- compiler/parser
import GHC.Parser.Lexer

import GHC.Parser.PostProcess (runECP_P)

fullStatement :: P (Maybe (LStmt GhcPs (LHsExpr GhcPs)))
fullStatement = parseStmt

fullImport :: P (LImportDecl GhcPs)
fullImport = parseImport

fullDeclaration :: P (OrdList (LHsDecl GhcPs))
fullDeclaration = fmap unitOL parseDeclaration

fullExpression :: P (LHsExpr GhcPs)
fullExpression = runECP_P =<< parseExpression

fullTypeSignature :: P (Located (OrdList (LHsDecl GhcPs)))
fullTypeSignature = fmap (noLoc . unitOL) parseTypeSignature

fullModule :: P (Located HsModule)
fullModule = parseModule
