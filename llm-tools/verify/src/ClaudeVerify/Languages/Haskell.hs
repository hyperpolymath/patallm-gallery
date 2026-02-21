-- |
-- Module      : ClaudeVerify.Languages.Haskell
-- Description : Haskell source code parser for verification
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- Parser for Haskell source code, extracting the relevant structure
-- for verification condition generation.
--
-- Haskell's type system provides many guarantees, so verification
-- focuses on:
--
-- * Partial functions (head, tail, fromJust)
-- * Pattern match exhaustiveness
-- * Refinement types (via LiquidHaskell annotations)
-- * QuickCheck properties
-- * Preconditions in documentation

module ClaudeVerify.Languages.Haskell
    ( -- * Parsing
      parseHaskell
    , parseHaskellFile

      -- * AST
    , HaskellAST(..)
    , toGenericAST
    ) where

import ClaudeVerify.Internal.AST

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type
type Parser = Parsec Void Text

-- | Haskell-specific AST
newtype HaskellAST = HaskellAST { unHaskellAST :: Module }
    deriving stock (Show)

-- | Parse Haskell source code
parseHaskell :: Text -> Either (ParseErrorBundle Text Void) HaskellAST
parseHaskell = parse (HaskellAST <$> haskellModule <* eof) "<input>"

-- | Parse a Haskell source file
parseHaskellFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) HaskellAST)
parseHaskellFile path = do
    content <- TIO.readFile path
    pure $ parse (HaskellAST <$> haskellModule <* eof) path content

-- | Convert to generic AST
toGenericAST :: HaskellAST -> Module
toGenericAST = unHaskellAST

-- | Lexer space consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a Haskell module
haskellModule :: Parser Module
haskellModule = do
    sc
    name <- optional (symbol "module" *> haskellModName <* symbol "where")
    decls <- many haskellDecl
    pure Module
        { modAnnotation = emptyAnn
        , modName = name
        , modDecls = decls
        }

-- | Parse module name
haskellModName :: Parser Text
haskellModName = lexeme $ do
    parts <- sepBy1 haskellConId (char '.')
    pure $ T.intercalate "." parts

-- | Parse a declaration
haskellDecl :: Parser Declaration
haskellDecl = choice
    [ try haskellTypeSig
    , try haskellFunDecl
    , try haskellDataDecl
    , haskellImport
    ]

-- | Parse type signature
haskellTypeSig :: Parser Declaration
haskellTypeSig = do
    name <- haskellVarId
    _ <- symbol "::"
    ty <- haskellType
    pure $ DeclConst emptyAnn (Ident name) ty (ExprLit emptyAnn LitUnit)

-- | Parse function declaration
haskellFunDecl :: Parser Declaration
haskellFunDecl = do
    name <- haskellVarId
    params <- many haskellPattern
    _ <- symbol "="
    body <- haskellExpr
    let fn = Function
            { fnAnnotation = emptyAnn
            , fnName = Ident name
            , fnGenerics = []
            , fnParams = []  -- Haskell has pattern matching, not typed params here
            , fnReturnType = TyInfer
            , fnBody = Just [StmtExpr emptyAnn body]
            , fnPreconditions = []
            , fnPostconditions = []
            }
    pure $ DeclFunction emptyAnn fn

-- | Parse data declaration
haskellDataDecl :: Parser Declaration
haskellDataDecl = do
    _ <- symbol "data"
    name <- haskellConId
    params <- many haskellVarId
    _ <- symbol "="
    constrs <- sepBy1 haskellConstr (symbol "|")
    pure $ DeclEnum emptyAnn (Ident name) (map Ident params) constrs

-- | Parse constructor
haskellConstr :: Parser (Ident, [Type])
haskellConstr = do
    name <- haskellConId
    tys <- many haskellAType
    pure (Ident name, tys)

-- | Parse import
haskellImport :: Parser Declaration
haskellImport = do
    _ <- symbol "import"
    _ <- optional (symbol "qualified")
    name <- haskellModName
    _ <- optional (symbol "as" *> haskellConId)
    _ <- optional (symbol "hiding" <|> symbol "(")
    -- Skip rest of import
    _ <- many (noneOf ['\n'])
    pure $ DeclUse emptyAnn (QualifiedIdent (T.split (== '.') name) "")

-- | Parse type
haskellType :: Parser Type
haskellType = do
    tys <- sepBy1 haskellBType (symbol "->")
    pure $ case tys of
        [t] -> t
        _   -> TyFun (init tys) (last tys)

-- | Parse btype (type application)
haskellBType :: Parser Type
haskellBType = do
    tys <- some haskellAType
    pure $ case tys of
        [t] -> t
        (t:ts) -> TyApp t ts
        [] -> TyUnit

-- | Parse atype (atomic type)
haskellAType :: Parser Type
haskellAType = choice
    [ TyTuple <$> parens (sepBy haskellType (symbol ","))
    , TyArray <$> (symbol "[" *> haskellType <* symbol "]") <*> pure Nothing
    , TyName . QualifiedIdent [] <$> haskellConId
    , TyName . QualifiedIdent [] <$> haskellVarId
    ]

-- | Parse pattern
haskellPattern :: Parser Pattern
haskellPattern = choice
    [ PatWildcard <$ symbol "_"
    , PatIdent . Ident <$> haskellVarId
    ]

-- | Parse expression (simplified)
haskellExpr :: Parser Expression
haskellExpr = do
    terms <- some haskellTerm
    pure $ case terms of
        [t] -> t
        (t:ts) -> foldl (ExprCall emptyAnn) t (map (:[]) ts)
        [] -> ExprLit emptyAnn LitUnit

-- | Parse term
haskellTerm :: Parser Expression
haskellTerm = choice
    [ try (ExprLit emptyAnn . LitInt <$> lexeme L.decimal)
    , try (ExprLit emptyAnn . LitFloat <$> lexeme L.float)
    , ExprLit emptyAnn . LitString <$> haskellString
    , ExprVar emptyAnn . QualifiedIdent [] <$> haskellVarId
    , ExprVar emptyAnn . QualifiedIdent [] <$> haskellConId
    ]

-- | Parse variable identifier
haskellVarId :: Parser Text
haskellVarId = lexeme $ do
    first <- lowerChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_' <|> char '\'')
    let ident = T.pack (first : rest)
    if ident `elem` reserved then fail "reserved" else pure ident
  where
    reserved = ["let", "in", "where", "case", "of", "if", "then", "else", "do", "module", "import", "data", "type", "class", "instance"]

-- | Parse constructor identifier
haskellConId :: Parser Text
haskellConId = lexeme $ do
    first <- upperChar
    rest <- many (alphaNumChar <|> char '_' <|> char '\'')
    pure $ T.pack (first : rest)

-- | Parse string literal
haskellString :: Parser Text
haskellString = do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    pure $ T.pack content

-- | Parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Empty annotation
emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing []
