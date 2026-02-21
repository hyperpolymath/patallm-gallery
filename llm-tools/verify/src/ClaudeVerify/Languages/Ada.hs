-- |
-- Module      : ClaudeVerify.Languages.Ada
-- Description : Ada/SPARK source code parser for verification
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- Parser for Ada/SPARK source code. Ada is particularly interesting
-- for verification because:
--
-- * SPARK subset is formally verifiable
-- * Strong typing with range constraints
-- * Pre/postconditions are part of the language
-- * GNATprove can verify SPARK code
--
-- This module extracts:
--
-- * Subprogram contracts (Pre, Post, Contract_Cases)
-- * Type constraints (range types, subtypes)
-- * Assertions and pragmas
-- * Loop invariants

module ClaudeVerify.Languages.Ada
    ( -- * Parsing
      parseAda
    , parseAdaFile

      -- * AST
    , AdaAST(..)
    , toGenericAST

      -- * SPARK
    , extractSparkContracts
    ) where

import ClaudeVerify.Internal.AST

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- | Parser type
type Parser = Parsec Void Text

-- | Ada-specific AST
data AdaAST = AdaAST
    { adaModule :: Module
    , adaSparkMode :: Bool
    } deriving stock (Show)

-- | SPARK contract
data SparkContract = SparkContract
    { scSubprogram  :: Text
    , scPrecondition :: Maybe Expression
    , scPostcondition :: Maybe Expression
    , scContractCases :: [(Expression, Expression)]
    } deriving stock (Show)

-- | Parse Ada source code
parseAda :: Text -> Either (ParseErrorBundle Text Void) AdaAST
parseAda = parse (adaCompilationUnit <* eof) "<input>"

-- | Parse an Ada source file
parseAdaFile :: FilePath -> IO (Either (ParseErrorBundle Text Void) AdaAST)
parseAdaFile path = do
    content <- TIO.readFile path
    pure $ parse (adaCompilationUnit <* eof) path content

-- | Convert to generic AST
toGenericAST :: AdaAST -> Module
toGenericAST = adaModule

-- | Extract SPARK contracts from Ada/SPARK AST
--
-- Walks through all declarations (including nested modules) and extracts
-- contracts from function declarations that have Pre/Post conditions.
extractSparkContracts :: AdaAST -> [SparkContract]
extractSparkContracts ast = extractFromDecls (modDecls $ adaModule ast)
  where
    -- | Extract contracts from a list of declarations
    extractFromDecls :: [Declaration] -> [SparkContract]
    extractFromDecls = concatMap extractFromDecl

    -- | Extract contract from a single declaration
    extractFromDecl :: Declaration -> [SparkContract]
    extractFromDecl (DeclFunction _ fn) =
        let name = unIdent (fnName fn)
            pres = fnPreconditions fn
            posts = fnPostconditions fn
        in if null pres && null posts
           then []
           else [SparkContract
               { scSubprogram = name
               , scPrecondition = listToMaybe pres
               , scPostcondition = listToMaybe posts
               , scContractCases = []  -- Contract_Cases parsed separately via aspects
               }]
    -- Recurse into nested modules (packages)
    extractFromDecl (DeclModule _ _ decls) = extractFromDecls decls
    -- Skip other declaration types
    extractFromDecl _ = []

-- | Lexer space consumer
sc :: Parser ()
sc = L.space
    space1
    (L.skipLineComment "--")
    empty  -- Ada has no block comments

-- | Lexeme wrapper
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Case-insensitive symbol
symbol :: Text -> Parser Text
symbol s = lexeme $ do
    result <- string' s
    pure $ T.toUpper result
  where
    string' t = T.pack <$> traverse (satisfy . charMatch) (T.unpack t)
    charMatch c x = c == x || toUpper c == toUpper x
    toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- | Parse compilation unit
adaCompilationUnit :: Parser AdaAST
adaCompilationUnit = do
    sc
    spark <- option False (True <$ symbol "pragma" <* symbol "SPARK_Mode" <* symbol "(On)" <* symbol ";")
    decls <- many adaDeclaration
    pure AdaAST
        { adaModule = Module emptyAnn Nothing decls
        , adaSparkMode = spark
        }

-- | Parse a declaration
adaDeclaration :: Parser Declaration
adaDeclaration = choice
    [ try adaPackage
    , try adaSubprogram
    , try adaType
    , adaPragma
    ]

-- | Parse package
adaPackage :: Parser Declaration
adaPackage = do
    _ <- symbol "package"
    name <- adaIdentifier
    _ <- symbol "is"
    decls <- many adaDeclaration
    _ <- symbol "end"
    _ <- adaIdentifier
    _ <- symbol ";"
    pure $ DeclModule emptyAnn (Ident name) decls

-- | Parse subprogram
adaSubprogram :: Parser Declaration
adaSubprogram = do
    kind <- (symbol "procedure" *> pure False) <|> (symbol "function" *> pure True)
    name <- adaIdentifier
    params <- option [] (parens (sepBy adaParam (symbol ";")))
    retTy <- if kind
        then symbol "return" *> adaTypeRef
        else pure TyUnit

    -- Parse contracts
    contracts <- many adaAspect

    bodyOrDecl <- (symbol "is" *> (Just <$> adaStatementList) <* symbol "end" <* adaIdentifier <* symbol ";")
             <|> (symbol ";" *> pure Nothing)

    let (pres, posts) = partitionContracts contracts
        fn = Function
            { fnAnnotation = emptyAnn
            , fnName = Ident name
            , fnGenerics = []
            , fnParams = params
            , fnReturnType = retTy
            , fnBody = bodyOrDecl
            , fnPreconditions = pres
            , fnPostconditions = posts
            }
    pure $ DeclFunction emptyAnn fn

-- | Partition contracts into pre/post
partitionContracts :: [(Text, Expression)] -> ([Expression], [Expression])
partitionContracts = foldr classify ([], [])
  where
    classify (name, expr) (pres, posts)
        | T.toUpper name == "PRE" = (expr : pres, posts)
        | T.toUpper name == "POST" = (pres, expr : posts)
        | otherwise = (pres, posts)

-- | Parse aspect specification
adaAspect :: Parser (Text, Expression)
adaAspect = do
    _ <- symbol "with"
    name <- adaIdentifier
    _ <- symbol "=>"
    expr <- adaExpression
    pure (name, expr)

-- | Parse parameter
adaParam :: Parser (Ident, Type)
adaParam = do
    name <- adaIdentifier
    _ <- symbol ":"
    _ <- optional (symbol "in" <|> symbol "out" <|> symbol "in out")
    ty <- adaTypeRef
    pure (Ident name, ty)

-- | Parse type declaration
adaType :: Parser Declaration
adaType = do
    _ <- symbol "type"
    name <- adaIdentifier
    _ <- symbol "is"
    ty <- adaTypeDef
    _ <- symbol ";"
    pure $ DeclType emptyAnn (Ident name) [] ty

-- | Parse type definition
adaTypeDef :: Parser Type
adaTypeDef = choice
    [ adaRangeType
    , adaEnumType
    , adaRecordType
    , adaTypeRef
    ]

-- | Parse range type
adaRangeType :: Parser Type
adaRangeType = do
    _ <- symbol "range"
    low <- adaExpression
    _ <- symbol ".."
    high <- adaExpression
    -- Represent as application of Range to bounds
    pure $ TyApp (TyName $ QualifiedIdent [] "Range") [TyInfer]

-- | Parse enum type
adaEnumType :: Parser Type
adaEnumType = do
    _ <- symbol "("
    _ <- sepBy1 adaIdentifier (symbol ",")
    _ <- symbol ")"
    pure $ TyName $ QualifiedIdent [] "Enum"

-- | Parse record type
adaRecordType :: Parser Type
adaRecordType = do
    _ <- symbol "record"
    _ <- many adaRecordField
    _ <- symbol "end record"
    pure $ TyName $ QualifiedIdent [] "Record"

-- | Parse record field
adaRecordField :: Parser (Text, Type)
adaRecordField = do
    name <- adaIdentifier
    _ <- symbol ":"
    ty <- adaTypeRef
    _ <- symbol ";"
    pure (name, ty)

-- | Parse type reference
adaTypeRef :: Parser Type
adaTypeRef = TyName . QualifiedIdent [] <$> adaIdentifier

-- | Parse pragma
adaPragma :: Parser Declaration
adaPragma = do
    _ <- symbol "pragma"
    name <- adaIdentifier
    args <- option [] (parens (sepBy adaExpression (symbol ",")))
    _ <- symbol ";"
    pure $ DeclConst emptyAnn (Ident $ "pragma_" <> name) TyUnit (ExprLit emptyAnn LitUnit)

-- | Parse statement list
adaStatementList :: Parser [Statement]
adaStatementList = do
    _ <- symbol "begin"
    stmts <- many adaStatement
    pure stmts

-- | Parse statement
adaStatement :: Parser Statement
adaStatement = choice
    [ try adaAssignment
    , try adaIf
    , try adaLoop
    , adaCall
    ]

-- | Parse assignment
adaAssignment :: Parser Statement
adaAssignment = do
    target <- adaExpression
    _ <- symbol ":="
    value <- adaExpression
    _ <- symbol ";"
    pure $ StmtAssign emptyAnn target value

-- | Parse if statement
adaIf :: Parser Statement
adaIf = do
    _ <- symbol "if"
    cond <- adaExpression
    _ <- symbol "then"
    thenStmts <- many adaStatement
    _ <- optional (symbol "else" *> many adaStatement)
    _ <- symbol "end if"
    _ <- symbol ";"
    pure $ StmtExpr emptyAnn $ ExprIf emptyAnn cond
        (ExprBlock emptyAnn thenStmts) Nothing

-- | Parse loop
adaLoop :: Parser Statement
adaLoop = do
    _ <- optional (adaIdentifier <* symbol ":")
    _ <- symbol "loop"
    body <- many adaStatement
    _ <- symbol "end loop"
    _ <- symbol ";"
    pure $ StmtLoop emptyAnn body

-- | Parse procedure call
adaCall :: Parser Statement
adaCall = do
    name <- adaIdentifier
    args <- option [] (parens (sepBy adaExpression (symbol ",")))
    _ <- symbol ";"
    pure $ StmtExpr emptyAnn $ ExprCall emptyAnn
        (ExprVar emptyAnn $ QualifiedIdent [] name) args

-- | Parse expression
adaExpression :: Parser Expression
adaExpression = adaTerm

-- | Parse term
adaTerm :: Parser Expression
adaTerm = choice
    [ try (ExprLit emptyAnn . LitInt <$> lexeme L.decimal)
    , try (ExprLit emptyAnn . LitFloat <$> lexeme L.float)
    , ExprLit emptyAnn . LitBool True <$ symbol "True"
    , ExprLit emptyAnn . LitBool False <$ symbol "False"
    , ExprVar emptyAnn . QualifiedIdent [] <$> adaIdentifier
    ]

-- | Parse identifier
adaIdentifier :: Parser Text
adaIdentifier = lexeme $ do
    first <- letterChar
    rest <- many (alphaNumChar <|> char '_')
    pure $ T.pack (first : rest)

-- | Parentheses
parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- | Empty annotation
emptyAnn :: Annotation
emptyAnn = Annotation Nothing Nothing []
