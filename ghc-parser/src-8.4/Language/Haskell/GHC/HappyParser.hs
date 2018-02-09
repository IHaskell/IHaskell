module Language.Haskell.GHC.HappyParser
    ( fullStatement
    , fullImport
    , fullDeclaration
    , fullExpression
    , fullTypeSignature
    , fullModule
    ) where

import Parser
import SrcLoc

-- compiler/hsSyn
import HsSyn

-- compiler/utils
import OrdList

-- compiler/parser
import RdrHsSyn
import Lexer

-- compiler/basicTypes
import RdrName

fullStatement :: P (Maybe (LStmt GhcPs (LHsExpr GhcPs)))
fullStatement = parseStmt

fullImport :: P (LImportDecl GhcPs)
fullImport = parseImport

fullDeclaration :: P (OrdList (LHsDecl GhcPs))
fullDeclaration = fmap unitOL parseDeclaration

fullExpression :: P (LHsExpr GhcPs)
fullExpression = parseExpression

fullTypeSignature :: P (Located (OrdList (LHsDecl GhcPs)))
fullTypeSignature = fmap (noLoc . unitOL) parseTypeSignature

fullModule :: P (Located (HsModule GhcPs))
fullModule = parseModule
