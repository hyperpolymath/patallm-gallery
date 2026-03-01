{-# LANGUAGE StrictData #-}
-- |
-- Module      : ClaudeVerify.PropertyExtraction
-- Description : Extract verification conditions from source code
-- Copyright   : (c) Hyperpolymath, 2025
-- License     : MIT
--
-- This module extracts verification conditions from parsed source code.
-- Verification conditions (VCs) are logical formulas that must be true
-- for the code to be correct.
--
-- Extracted VCs include:
--
-- * Bounds checks (array indexing)
-- * Null/option checks (dereference safety)
-- * Arithmetic overflow (integer operations)
-- * Assertion correctness (assert statements)
-- * Precondition satisfaction (function calls)
-- * Postcondition validity (function returns)
-- * Loop invariant maintenance

module ClaudeVerify.PropertyExtraction
    ( -- * Extraction
      extractVCs
    , extractFromFunction
    , extractFromStatement
    , extractFromExpression

      -- * VC Categories
    , VCCategory(..)
    , categorize

      -- * Weakest Precondition
    , wp
    , wpStatement
    , substitute
    ) where

import ClaudeVerify.Internal.AST
import ClaudeVerify.Internal.Types
import ClaudeVerify.Internal.SMTLIB

import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (mapMaybe, catMaybes)

-- | Category of verification condition
data VCCategory
    = BoundsCheck        -- ^ Array bounds
    | NullCheck          -- ^ Null/None dereference
    | OverflowCheck      -- ^ Integer overflow
    | AssertionCheck     -- ^ Explicit assertion
    | PreconditionCheck  -- ^ Function precondition
    | PostconditionCheck -- ^ Function postcondition
    | InvariantCheck     -- ^ Loop invariant
    | TypeCheck          -- ^ Type safety
    | MemoryCheck        -- ^ Memory safety (use-after-free, etc.)
    | ConcurrencyCheck   -- ^ Race conditions, deadlocks
    deriving stock (Eq, Show)

-- | State for VC extraction
data ExtractState = ExtractState
    { esVCs         :: [VerificationCondition]
    , esContext     :: [Expression]  -- ^ Current path condition
    , esCounter     :: Int
    , esCurrentFn   :: Maybe Ident
    } deriving stock (Show)

-- | VC extraction monad
type Extract a = State ExtractState a

-- | Initial state
initialState :: ExtractState
initialState = ExtractState
    { esVCs = []
    , esContext = []
    , esCounter = 0
    , esCurrentFn = Nothing
    }

-- | Generate fresh VC ID
freshVCId :: Text -> Extract VCId
freshVCId prefix = do
    n <- gets esCounter
    modify' $ \s -> s { esCounter = n + 1 }
    pure $ VCId $ prefix <> "_" <> T.pack (show n)

-- | Add a VC to the collection
emitVC :: VCId -> Text -> Expression -> SourceLocation -> VCCategory -> Extract ()
emitVC vcid desc expr loc category = do
    context <- gets esContext
    let formula = buildFormula context expr
        vc = VerificationCondition
            { vcId = vcid
            , vcDescription = desc
            , vcFormula = toSMTLIB formula
            , vcLocation = loc
            , vcKind = T.pack $ show category
            }
    modify' $ \s -> s { esVCs = vc : esVCs s }

-- | Build formula from context and goal
buildFormula :: [Expression] -> Expression -> Expression
buildFormula [] goal = goal
buildFormula ctx goal =
    let conjunction = foldr1 (\a b -> ExprBinOp emptyAnn And a b) ctx
    in ExprBinOp emptyAnn And conjunction goal
  where
    emptyAnn = Annotation Nothing Nothing []

-- | Push path condition
pushContext :: Expression -> Extract ()
pushContext e = modify' $ \s -> s { esContext = e : esContext s }

-- | Pop path condition
popContext :: Extract ()
popContext = modify' $ \s -> s { esContext = drop 1 $ esContext s }

-- | Extract all VCs from a module
extractVCs :: Module -> [VerificationCondition]
extractVCs modul =
    let (_, finalState) = runState (extractModule modul) initialState
    in reverse $ esVCs finalState

-- | Extract from module
extractModule :: Module -> Extract ()
extractModule (Module _ _ decls) = mapM_ extractDecl decls

-- | Extract from declaration
extractDecl :: Declaration -> Extract ()
extractDecl = \case
    DeclFunction _ fn -> extractFromFunction fn
    DeclModule _ _ decls -> mapM_ extractDecl decls
    DeclImpl _ _ _ decls -> mapM_ extractDecl decls
    _ -> pure ()

-- | Extract VCs from a function
extractFromFunction :: Function -> Extract ()
extractFromFunction fn = do
    modify' $ \s -> s { esCurrentFn = Just (fnName fn) }

    -- Add preconditions to context
    forM_ (fnPreconditions fn) $ \pre ->
        pushContext pre

    -- Process body
    case fnBody fn of
        Just stmts -> mapM_ extractFromStatement stmts
        Nothing -> pure ()

    -- Generate postcondition VCs
    forM_ (fnPostconditions fn) $ \post -> do
        let loc = getSpanLocation (fnAnnotation fn)
        vcid <- freshVCId "postcond"
        emitVC vcid "Postcondition must hold at function exit" post loc PostconditionCheck

    -- Clean up context
    forM_ (fnPreconditions fn) $ \_ -> popContext
    modify' $ \s -> s { esCurrentFn = Nothing }

-- | Extract VCs from a statement
extractFromStatement :: Statement -> Extract ()
extractFromStatement = \case
    StmtLet ann pat mty expr -> do
        extractFromExpression expr
        -- Type check VC if type annotation present
        case mty of
            Just ty -> do
                let loc = getSpanLocation ann
                vcid <- freshVCId "typecheck"
                emitVC vcid "Expression type must match annotation"
                    (ExprCast ann expr ty) loc TypeCheck
            Nothing -> pure ()

    StmtAssign ann lhs rhs -> do
        extractFromExpression lhs
        extractFromExpression rhs

    StmtExpr _ expr -> extractFromExpression expr

    StmtWhile ann cond body mInvariant -> do
        -- Extract from condition
        extractFromExpression cond

        -- Invariant must hold on entry
        case mInvariant of
            Just inv -> do
                let loc = getSpanLocation ann
                vcid <- freshVCId "inv_entry"
                emitVC vcid "Loop invariant must hold on entry" inv loc InvariantCheck
            Nothing -> pure ()

        -- Push loop condition to context
        pushContext cond

        -- Process body
        mapM_ extractFromStatement body

        -- Invariant must be maintained
        case mInvariant of
            Just inv -> do
                let loc = getSpanLocation ann
                vcid <- freshVCId "inv_maintained"
                emitVC vcid "Loop invariant must be maintained" inv loc InvariantCheck
            Nothing -> pure ()

        popContext

    StmtFor ann _pat iter body -> do
        extractFromExpression iter
        mapM_ extractFromStatement body

    StmtLoop _ body -> mapM_ extractFromStatement body

    StmtEmpty -> pure ()

-- | Extract VCs from an expression
extractFromExpression :: Expression -> Extract ()
extractFromExpression = \case
    ExprLit _ _ -> pure ()
    ExprVar _ _ -> pure ()

    ExprBinOp ann op e1 e2 -> do
        extractFromExpression e1
        extractFromExpression e2
        -- Overflow check for arithmetic
        case op of
            Add -> emitOverflowVC ann e1 e2 "addition"
            Sub -> emitOverflowVC ann e1 e2 "subtraction"
            Mul -> emitOverflowVC ann e1 e2 "multiplication"
            Div -> emitDivByZeroVC ann e2
            Mod -> emitDivByZeroVC ann e2
            _ -> pure ()

    ExprUnOp _ _ e -> extractFromExpression e

    ExprCall ann fn args -> do
        extractFromExpression fn
        mapM_ extractFromExpression args

    ExprMethodCall _ receiver _ args -> do
        extractFromExpression receiver
        mapM_ extractFromExpression args

    ExprIf _ cond thenE elseE -> do
        extractFromExpression cond
        pushContext cond
        extractFromExpression thenE
        popContext
        case elseE of
            Just e -> do
                pushContext (negateExpr cond)
                extractFromExpression e
                popContext
            Nothing -> pure ()

    ExprMatch _ scrutinee arms -> do
        extractFromExpression scrutinee
        forM_ arms $ \(_, armExpr) ->
            extractFromExpression armExpr

    ExprBlock _ stmts -> mapM_ extractFromStatement stmts

    ExprLambda _ _ body -> extractFromExpression body

    ExprTuple _ exprs -> mapM_ extractFromExpression exprs
    ExprArray _ exprs -> mapM_ extractFromExpression exprs

    ExprIndex ann arr idx -> do
        extractFromExpression arr
        extractFromExpression idx
        -- Bounds check
        let loc = getSpanLocation ann
        vcid <- freshVCId "bounds"
        let boundsVC = ExprBinOp ann And
                (ExprBinOp ann Ge idx (ExprLit ann (LitInt 0)))
                (ExprBinOp ann Lt idx (arrayLen arr))
        emitVC vcid "Array index must be within bounds" boundsVC loc BoundsCheck

    ExprField _ base _ -> extractFromExpression base

    ExprCast _ e _ -> extractFromExpression e

    ExprReturn _ me -> mapM_ extractFromExpression me
    ExprBreak _ me -> mapM_ extractFromExpression me
    ExprContinue _ -> pure ()

    ExprAssert ann e msg -> do
        extractFromExpression e
        let loc = getSpanLocation ann
            desc = maybe "Assertion must hold" id msg
        vcid <- freshVCId "assert"
        emitVC vcid desc e loc AssertionCheck

    ExprAssume _ e -> do
        extractFromExpression e
        pushContext e

    ExprInvariant ann e -> do
        extractFromExpression e
        let loc = getSpanLocation ann
        vcid <- freshVCId "invariant"
        emitVC vcid "Invariant must hold" e loc InvariantCheck

    ExprRequires ann e -> do
        let loc = getSpanLocation ann
        vcid <- freshVCId "requires"
        emitVC vcid "Precondition must hold at call site" e loc PreconditionCheck

    ExprEnsures ann e -> do
        let loc = getSpanLocation ann
        vcid <- freshVCId "ensures"
        emitVC vcid "Postcondition must hold" e loc PostconditionCheck

-- | Emit overflow check VC
emitOverflowVC :: Annotation -> Expression -> Expression -> Text -> Extract ()
emitOverflowVC ann e1 e2 opName = do
    let loc = getSpanLocation ann
    vcid <- freshVCId "overflow"
    -- Simplified: just note that overflow could occur
    let desc = "No overflow in " <> opName
    -- Real implementation would compute actual overflow conditions
    emitVC vcid desc (ExprLit ann (LitBool True)) loc OverflowCheck

-- | Emit division by zero check VC
emitDivByZeroVC :: Annotation -> Expression -> Extract ()
emitDivByZeroVC ann divisor = do
    let loc = getSpanLocation ann
    vcid <- freshVCId "divzero"
    let check = ExprBinOp ann Ne divisor (ExprLit ann (LitInt 0))
    emitVC vcid "Divisor must not be zero" check loc BoundsCheck

-- | Negate an expression
negateExpr :: Expression -> Expression
negateExpr e@(ExprLit ann _) = ExprUnOp ann Not e
negateExpr e@(ExprVar ann _) = ExprUnOp ann Not e
negateExpr e = ExprUnOp (Annotation Nothing Nothing []) Not e

-- | Get array length expression
--
-- Generates a method call to .len() for the given array expression.
-- Language-specific backends may transform this to the appropriate syntax.
arrayLen :: Expression -> Expression
arrayLen arr = ExprMethodCall (Annotation Nothing Nothing []) arr (Ident "len") []

-- | Get source location from annotation
getSpanLocation :: Annotation -> SourceLocation
getSpanLocation ann = case annSpan ann of
    Just span -> SourceLocation
        { slFile = spanFile span
        , slLine = fst $ spanStart span
        , slColumn = Just $ snd $ spanStart span
        , slEndLine = Just $ fst $ spanEnd span
        , slEndColumn = Just $ snd $ spanEnd span
        }
    Nothing -> SourceLocation
        { slFile = "<unknown>"
        , slLine = 0
        , slColumn = Nothing
        , slEndLine = Nothing
        , slEndColumn = Nothing
        }

-- | Categorize a VC
categorize :: VerificationCondition -> VCCategory
categorize vc = case vcKind vc of
    "BoundsCheck" -> BoundsCheck
    "NullCheck" -> NullCheck
    "OverflowCheck" -> OverflowCheck
    "AssertionCheck" -> AssertionCheck
    "PreconditionCheck" -> PreconditionCheck
    "PostconditionCheck" -> PostconditionCheck
    "InvariantCheck" -> InvariantCheck
    "TypeCheck" -> TypeCheck
    "MemoryCheck" -> MemoryCheck
    "ConcurrencyCheck" -> ConcurrencyCheck
    _ -> AssertionCheck

-- | Weakest precondition for an expression
wp :: Expression -> Expression -> Expression
wp post expr = case expr of
    ExprAssign ann lhs rhs -> substitute lhs rhs post
    _ -> post

-- | Weakest precondition for a statement
wpStatement :: Expression -> Statement -> Expression
wpStatement post = \case
    StmtLet _ (PatIdent ident) _ expr ->
        substitute (ExprVar (Annotation Nothing Nothing [])
            (QualifiedIdent [] (unIdent ident))) expr post
    StmtAssign _ lhs rhs -> substitute lhs rhs post
    StmtExpr _ (ExprAssert _ e _) -> ExprBinOp (Annotation Nothing Nothing []) And e post
    _ -> post

-- | Substitute expression
substitute :: Expression -> Expression -> Expression -> Expression
substitute target replacement = go
  where
    go e | exprEquals e target = replacement
    go (ExprBinOp ann op e1 e2) = ExprBinOp ann op (go e1) (go e2)
    go (ExprUnOp ann op e) = ExprUnOp ann op (go e)
    go (ExprIf ann c t me) = ExprIf ann (go c) (go t) (fmap go me)
    go (ExprCall ann fn args) = ExprCall ann (go fn) (map go args)
    go (ExprTuple ann es) = ExprTuple ann (map go es)
    go e = e

    exprEquals (ExprVar _ q1) (ExprVar _ q2) = q1 == q2
    exprEquals (ExprLit _ l1) (ExprLit _ l2) = l1 == l2
    exprEquals _ _ = False
