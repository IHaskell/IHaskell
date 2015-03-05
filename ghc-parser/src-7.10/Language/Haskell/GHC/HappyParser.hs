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

fullStatement :: P (Maybe (LStmt RdrName (LHsExpr RdrName)))
fullStatement = parseStmt

fullImport :: P (LImportDecl RdrName)
fullImport = parseImport

fullDeclaration :: P (OrdList (LHsDecl RdrName))
fullDeclaration = parseDeclaration

fullExpression :: P (LHsExpr RdrName)
fullExpression = parseExpression

fullTypeSignature :: P (Located (OrdList (LHsDecl RdrName)))
fullTypeSignature = parseTypeSignature

fullModule :: P (Located (HsModule RdrName))
fullModule = parseModule
