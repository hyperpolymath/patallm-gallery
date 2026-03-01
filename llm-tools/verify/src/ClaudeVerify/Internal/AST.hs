{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.Internal.AST
-- Description : Language-agnostic AST for verification condition extraction
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- A simplified, language-agnostic AST used for extracting verification
-- conditions. This is not meant to be a complete representation of any
-- particular language, but rather captures the semantic structure needed
-- for verification.

module ClaudeVerify.Internal.AST
    ( -- * AST Types
      Module(..)
    , Declaration(..)
    , Function(..)
    , Statement(..)
    , Expression(..)
    , Type(..)
    , Pattern(..)
    , Literal(..)
    , BinOp(..)
    , UnOp(..)

      -- * Annotations
    , Annotation(..)
    , Span(..)

      -- * Identifiers
    , Ident(..)
    , QualifiedIdent(..)
    ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | Source span
data Span = Span
    { spanFile   :: FilePath
    , spanStart  :: (Int, Int)  -- ^ (line, column)
    , spanEnd    :: (Int, Int)
    } deriving stock (Eq, Show, Generic)

-- | Annotation attached to AST nodes
data Annotation = Annotation
    { annSpan       :: Maybe Span
    , annDocComment :: Maybe Text
    , annAttributes :: [(Text, Text)]  -- ^ Language-specific attributes
    } deriving stock (Eq, Show, Generic)

-- | Simple identifier
newtype Ident = Ident { unIdent :: Text }
    deriving stock (Eq, Ord, Show, Generic)

-- | Qualified identifier (e.g., module::function)
data QualifiedIdent = QualifiedIdent
    { qiModule :: [Text]
    , qiName   :: Text
    } deriving stock (Eq, Ord, Show, Generic)

-- | Binary operators
data BinOp
    = Add | Sub | Mul | Div | Mod
    | And | Or | Xor
    | Eq | Ne | Lt | Le | Gt | Ge
    | Shl | Shr
    | BitAnd | BitOr | BitXor
    deriving stock (Eq, Show, Generic)

-- | Unary operators
data UnOp
    = Neg | Not | BitNot | Deref | Ref
    deriving stock (Eq, Show, Generic)

-- | Literal values
data Literal
    = LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitChar Char
    | LitString Text
    | LitUnit
    deriving stock (Eq, Show, Generic)

-- | Type representation (simplified)
data Type
    = TyName QualifiedIdent
    | TyApp Type [Type]              -- ^ Type application (generics)
    | TyFun [Type] Type              -- ^ Function type
    | TyRef Type                     -- ^ Reference type
    | TyMutRef Type                  -- ^ Mutable reference
    | TyArray Type (Maybe Expression) -- ^ Array with optional size
    | TyTuple [Type]
    | TyUnit
    | TyInfer                        -- ^ Type to be inferred
    deriving stock (Eq, Show, Generic)

-- | Pattern for destructuring
data Pattern
    = PatWildcard
    | PatIdent Ident
    | PatLiteral Literal
    | PatTuple [Pattern]
    | PatConstructor QualifiedIdent [Pattern]
    | PatOr Pattern Pattern
    | PatGuard Pattern Expression
    deriving stock (Eq, Show, Generic)

-- | Expressions
data Expression
    = ExprLit Annotation Literal
    | ExprVar Annotation QualifiedIdent
    | ExprBinOp Annotation BinOp Expression Expression
    | ExprUnOp Annotation UnOp Expression
    | ExprCall Annotation Expression [Expression]
    | ExprMethodCall Annotation Expression Ident [Expression]
    | ExprIf Annotation Expression Expression (Maybe Expression)
    | ExprMatch Annotation Expression [(Pattern, Expression)]
    | ExprBlock Annotation [Statement]
    | ExprLambda Annotation [(Pattern, Maybe Type)] Expression
    | ExprTuple Annotation [Expression]
    | ExprArray Annotation [Expression]
    | ExprIndex Annotation Expression Expression
    | ExprField Annotation Expression Ident
    | ExprCast Annotation Expression Type
    | ExprReturn Annotation (Maybe Expression)
    | ExprBreak Annotation (Maybe Expression)
    | ExprContinue Annotation
    -- Verification-specific
    | ExprAssert Annotation Expression (Maybe Text)    -- ^ assert expr, msg
    | ExprAssume Annotation Expression                 -- ^ assume expr
    | ExprInvariant Annotation Expression              -- ^ loop invariant
    | ExprRequires Annotation Expression               -- ^ precondition
    | ExprEnsures Annotation Expression                -- ^ postcondition
    deriving stock (Eq, Show, Generic)

-- | Statements
data Statement
    = StmtLet Annotation Pattern (Maybe Type) Expression
    | StmtAssign Annotation Expression Expression
    | StmtExpr Annotation Expression
    | StmtWhile Annotation Expression [Statement] (Maybe Expression)  -- ^ invariant
    | StmtFor Annotation Pattern Expression [Statement]
    | StmtLoop Annotation [Statement]
    | StmtEmpty
    deriving stock (Eq, Show, Generic)

-- | Function definition
data Function = Function
    { fnAnnotation  :: Annotation
    , fnName        :: Ident
    , fnGenerics    :: [Ident]
    , fnParams      :: [(Ident, Type)]
    , fnReturnType  :: Type
    , fnBody        :: Maybe [Statement]  -- ^ Nothing for declarations
    , fnPreconditions  :: [Expression]    -- ^ requires clauses
    , fnPostconditions :: [Expression]    -- ^ ensures clauses
    } deriving stock (Eq, Show, Generic)

-- | Top-level declarations
data Declaration
    = DeclFunction Annotation Function
    | DeclStruct Annotation Ident [Ident] [(Ident, Type)]
    | DeclEnum Annotation Ident [Ident] [(Ident, [Type])]
    | DeclTrait Annotation Ident [Ident] [Declaration]
    | DeclImpl Annotation Type (Maybe Type) [Declaration]  -- ^ impl Type for Trait
    | DeclConst Annotation Ident Type Expression
    | DeclStatic Annotation Ident Type Expression
    | DeclType Annotation Ident [Ident] Type               -- ^ type alias
    | DeclUse Annotation QualifiedIdent
    | DeclModule Annotation Ident [Declaration]
    deriving stock (Eq, Show, Generic)

-- | A module (compilation unit)
data Module = Module
    { modAnnotation :: Annotation
    , modName       :: Maybe Text
    , modDecls      :: [Declaration]
    } deriving stock (Eq, Show, Generic)
