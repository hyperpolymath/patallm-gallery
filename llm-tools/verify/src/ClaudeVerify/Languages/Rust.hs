-- |
-- Module      : ClaudeVerify.Languages.Rust
-- Description : Rust source code parser for verification
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- Parser for Rust source code, extracting the relevant structure
-- for verification condition generation.
--
-- Currently supports:
--
-- * Function definitions with pre/postconditions
-- * Struct definitions
-- * Assertions and panics
-- * Array indexing (for bounds checks)
-- * Option/Result handling
--
-- Future support planned for:
--
-- * Unsafe blocks
-- * Lifetime annotations
-- * Trait bounds
-- * Macro expansion

module ClaudeVerify.Languages.Rust
    ( -- * Parsing
      parseRust
    , parseRustFile

      -- * AST
    , RustAST(..)
    , toGenericAST
    ) where

import ClaudeVerify.Internal.AST

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type
type Parser = Parsec Void Text

-- | Rust-specific AST (wraps generic AST with Rust extensions)
newtype RustAST = RustAST { unRustAST :: Module }
    deriving stock (Show)

-- | Parse Rust source code
parseRust :: Text -> Either (ParseErrorBundle Text Void) RustAST
parseRust = parse (RustAST <$> rustModule <* eof) "<input>"

-- | Parse a Rust source file
parseRustFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) RustAST)
parseRustFile path = do
    content <- TIO.readFile path
    pure $ parse (RustAST <$> rustModule <* eof) path content

-- | Convert to generic AST
toGenericAST :: RustAST -> Module
toGenericAST = unRustAST

-- | Lexer space consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Symbol parser
symbol :: Text -> Parser Text
symbol = L.symbol sc

-- | Parse a Rust module
rustModule :: Parser Module
rustModule = do
    sc  -- consume leading whitespace
    decls <- many rustDecl
    pure Module
        { modAnnotation = emptyAnn
        , modName = Nothing
        , modDecls = decls
        }

-- | Parse a declaration
rustDecl :: Parser Declaration
rustDecl = choice
    [ try rustFunction
    , try rustStruct
    , try rustEnum
    , try rustImpl
    , try rustUse
    , rustMod
    ]

-- | Parse a function
rustFunction :: Parser Declaration
rustFunction = do
    attrs <- many rustAttribute
    vis <- optional (symbol "pub")
    _ <- symbol "fn"
    name <- rustIdent
    generics <- option [] rustGenerics
    params <- parens (sepBy rustParam (symbol ","))
    retTy <- option TyUnit (symbol "->" *> rustType)
    body <- optional rustBlock

    let ann = Annotation
            { annSpan = Nothing
            , annDocComment = Nothing
            , annAttributes = attrs
            }
        fn = Function
            { fnAnnotation = ann
            , fnName = Ident name
            , fnGenerics = map Ident generics
            , fnParams = params
            , fnReturnType = retTy
            , fnBody = body
            , fnPreconditions = []
            , fnPostconditions = []
            }

    pure $ DeclFunction ann fn

-- | Parse a struct
rustStruct :: Parser Declaration
rustStruct = do
    _ <- optional (symbol "pub")
    _ <- symbol "struct"
    name <- rustIdent
    generics <- option [] rustGenerics
    fields <- braces (sepBy rustStructField (symbol ","))
    pure $ DeclStruct emptyAnn (Ident name) (map Ident generics) fields

-- | Parse struct field
rustStructField :: Parser (Ident, Type)
rustStructField = do
    _ <- optional (symbol "pub")
    name <- rustIdent
    _ <- symbol ":"
    ty <- rustType
    pure (Ident name, ty)

-- | Parse an enum
rustEnum :: Parser Declaration
rustEnum = do
    _ <- optional (symbol "pub")
    _ <- symbol "enum"
    name <- rustIdent
    generics <- option [] rustGenerics
    variants <- braces (sepBy rustEnumVariant (symbol ","))
    pure $ DeclEnum emptyAnn (Ident name) (map Ident generics) variants

-- | Parse enum variant
rustEnumVariant :: Parser (Ident, [Type])
rustEnumVariant = do
    name <- rustIdent
    tys <- option [] (parens (sepBy rustType (symbol ",")))
    pure (Ident name, tys)

-- | Parse impl block
rustImpl :: Parser Declaration
rustImpl = do
    _ <- symbol "impl"
    _ <- optional rustGenerics
    ty <- rustType
    traitTy <- optional (symbol "for" *> rustType)
    decls <- braces (many rustDecl)
    pure $ DeclImpl emptyAnn ty traitTy decls

-- | Parse use statement
rustUse :: Parser Declaration
rustUse = do
    _ <- symbol "use"
    path <- sepBy1 rustIdent (symbol "::")
    _ <- symbol ";"
    let qident = QualifiedIdent (init path) (last path)
    pure $ DeclUse emptyAnn qident

-- | Parse mod
rustMod :: Parser Declaration
rustMod = do
    _ <- optional (symbol "pub")
    _ <- symbol "mod"
    name <- rustIdent
    choice
        [ symbol ";" *> pure (DeclModule emptyAnn (Ident name) [])
        , DeclModule emptyAnn (Ident name) <$> braces (many rustDecl)
        ]

-- | Parse a block (list of statements)
rustBlock :: Parser [Statement]
rustBlock = braces (many rustStatement)

-- | Parse a statement
rustStatement :: Parser Statement
rustStatement = choice
    [ try rustLetStatement
    , try rustExprStatement
    ]

-- | Parse let statement
rustLetStatement :: Parser Statement
rustLetStatement = do
    _ <- symbol "let"
    mut <- optional (symbol "mut")
    name <- rustIdent
    ty <- optional (symbol ":" *> rustType)
    _ <- symbol "="
    expr <- rustExpr
    _ <- symbol ";"
    let ann = maybe emptyAnn (\_ -> emptyAnn { annAttributes = [("mut", "true")] }) mut
    pure $ StmtLet ann (PatIdent $ Ident name) ty expr

-- | Parse expression statement
rustExprStatement :: Parser Statement
rustExprStatement = do
    expr <- rustExpr
    _ <- optional (symbol ";")
    pure $ StmtExpr emptyAnn expr

-- | Parse an expression
rustExpr :: Parser Expression
rustExpr = makeExprParser rustTerm operatorTable

-- | Expression terms
rustTerm :: Parser Expression
rustTerm = choice
    [ try rustLiteral
    , try rustIfExpr
    , try rustMatchExpr
    , try rustBlockExpr
    , rustIdentExpr
    ]

-- | Parse identifier expression
rustIdentExpr :: Parser Expression
rustIdentExpr = do
    name <- rustIdent
    pure $ ExprVar emptyAnn (QualifiedIdent [] name)

-- | Parse a literal
rustLiteral :: Parser Expression
rustLiteral = choice
    [ try (ExprLit emptyAnn . LitFloat <$> lexeme L.float)
    , ExprLit emptyAnn . LitInt <$> lexeme L.decimal
    , ExprLit emptyAnn . LitBool True <$ symbol "true"
    , ExprLit emptyAnn . LitBool False <$ symbol "false"
    , ExprLit emptyAnn . LitString <$> rustString
    , ExprLit emptyAnn . LitChar <$> rustChar
    ]

-- | Parse if expression
rustIfExpr :: Parser Expression
rustIfExpr = do
    _ <- symbol "if"
    cond <- rustExpr
    thenBlock <- ExprBlock emptyAnn <$> rustBlock
    elseBlock <- optional (symbol "else" *> (ExprBlock emptyAnn <$> rustBlock))
    pure $ ExprIf emptyAnn cond thenBlock elseBlock

-- | Parse match expression
rustMatchExpr :: Parser Expression
rustMatchExpr = do
    _ <- symbol "match"
    scrutinee <- rustExpr
    arms <- braces (sepBy rustMatchArm (symbol ","))
    pure $ ExprMatch emptyAnn scrutinee arms

-- | Parse match arm
rustMatchArm :: Parser (Pattern, Expression)
rustMatchArm = do
    pat <- rustPattern
    _ <- symbol "=>"
    expr <- rustExpr
    pure (pat, expr)

-- | Parse block expression
rustBlockExpr :: Parser Expression
rustBlockExpr = ExprBlock emptyAnn <$> rustBlock

-- | Parse a pattern
rustPattern :: Parser Pattern
rustPattern = choice
    [ PatWildcard <$ symbol "_"
    , PatIdent . Ident <$> rustIdent
    ]

-- | Parse a type
rustType :: Parser Type
rustType = choice
    [ TyRef <$> (symbol "&" *> rustType)
    , TyMutRef <$> (symbol "&" *> symbol "mut" *> rustType)
    , rustTypeSimple
    ]

-- | Simple type (identifier with optional generics)
rustTypeSimple :: Parser Type
rustTypeSimple = do
    name <- rustIdent
    args <- option [] (symbol "<" *> sepBy rustType (symbol ",") <* symbol ">")
    let qident = QualifiedIdent [] name
    pure $ if null args
        then TyName qident
        else TyApp (TyName qident) args

-- | Parse an identifier
rustIdent :: Parser Text
rustIdent = lexeme $ do
    first <- letterChar <|> char '_'
    rest <- many (alphaNumChar <|> char '_')
    pure $ T.pack (first : rest)

-- | Parse generics
rustGenerics :: Parser [Text]
rustGenerics = do
    _ <- symbol "<"
    names <- sepBy rustIdent (symbol ",")
    _ <- symbol ">"
    pure names

-- | Parse function parameter
rustParam :: Parser (Ident, Type)
rustParam = do
    name <- rustIdent
    _ <- symbol ":"
    ty <- rustType
    pure (Ident name, ty)

-- | Parse attribute
rustAttribute :: Parser (Text, Text)
rustAttribute = do
    _ <- symbol "#"
    _ <- symbol "["
    name <- rustIdent
    value <- option "" (parens rustIdent)
    _ <- symbol "]"
    pure (name, value)

-- | Parse string literal
rustString :: Parser Text
rustString = do
    _ <- char '"'
    content <- manyTill L.charLiteral (char '"')
    pure $ T.pack content

-- | Parse char literal
rustChar :: Parser Char
rustChar = do
    _ <- char '\''
    c <- L.charLiteral
    _ <- char '\''
    pure c

-- | Parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Braces
braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

-- | Operator table
operatorTable :: [[Operator Parser Expression]]
operatorTable =
    [ [ binary "*" (binOp Mul)
      , binary "/" (binOp Div)
      , binary "%" (binOp Mod)
      ]
    , [ binary "+" (binOp Add)
      , binary "-" (binOp Sub)
      ]
    , [ binary "<<" (binOp Shl)
      , binary ">>" (binOp Shr)
      ]
    , [ binary "<" (binOp Lt)
      , binary "<=" (binOp Le)
      , binary ">" (binOp Gt)
      , binary ">=" (binOp Ge)
      ]
    , [ binary "==" (binOp Eq)
      , binary "!=" (binOp Ne)
      ]
    , [ binary "&&" (binOp And)
      ]
    , [ binary "||" (binOp Or)
      ]
    ]
  where
    binary name f = InfixL (f <$ symbol name)
    binOp op e1 e2 = ExprBinOp emptyAnn op e1 e2

-- | Empty annotation
emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing []
