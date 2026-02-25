module Lib where
import Data.HashMap.Strict as H (HashMap, empty, fromList, insert, lookup, union)


--- Data Types
--- ----------

--- ### Environments and Results

type Env  = H.HashMap String Val
type PEnv = H.HashMap String Stmt

type Result = (String, PEnv, Env)

--- ### Values

data Val = IntVal Int
         | BoolVal Bool
         | CloVal [String] Exp Env
         | ExnVal String
    deriving (Eq)

instance Show Val where
    show (IntVal i) = show i
    show (BoolVal i) = show i
    show (CloVal xs body env) = "<" ++ show xs   ++ ", "
                                    ++ show body ++ ", "
                                    ++ show env  ++ ">"
    show (ExnVal s) = "exn: " ++ s

--- ### Expressions

data Exp = IntExp Int
         | BoolExp Bool
         | FunExp [String] Exp
         | LetExp [(String,Exp)] Exp
         | AppExp Exp [Exp]
         | IfExp Exp Exp Exp
         | IntOpExp String Exp Exp
         | BoolOpExp String Exp Exp
         | CompOpExp String Exp Exp
         | VarExp String
    deriving (Show, Eq)

--- ### Statements

data Stmt = SetStmt String Exp
          | PrintStmt Exp
          | QuitStmt
          | IfStmt Exp Stmt Stmt
          | ProcedureStmt String [String] Stmt
          | CallStmt String [Exp]
          | SeqStmt [Stmt]
    deriving (Show, Eq)

--- Primitive Functions
--- -------------------

intOps :: H.HashMap String (Int -> Int -> Int)
intOps = H.fromList [ ("+", (+))
                    , ("-", (-))
                    , ("*", (*))
                    , ("/", (div))
                    ]

boolOps :: H.HashMap String (Bool -> Bool -> Bool)
boolOps = H.fromList [ ("and", (&&))
                     , ("or", (||))
                     ]

compOps :: H.HashMap String (Int -> Int -> Bool)
compOps = H.fromList [ ("<", (<))
                     , (">", (>))
                     , ("<=", (<=))
                     , (">=", (>=))
                     , ("/=", (/=))
                     , ("==", (==))
                     ]

--- Problems
--- ========

--- Lifting Functions
--- -----------------

liftIntOp :: (Int -> Int -> Int) -> Val -> Val -> Val
liftIntOp op (IntVal x) (IntVal y) = IntVal $ op x y
liftIntOp _ _ _ = ExnVal "Cannot lift"

liftBoolOp :: (Bool -> Bool -> Bool) -> Val -> Val -> Val
liftBoolOp op (BoolVal x) (BoolVal y) = BoolVal (op x y)
liftBoolOp _ _ _ = ExnVal "Cannot lift"

liftCompOp :: (Int -> Int -> Bool) -> Val -> Val -> Val
liftCompOp op (IntVal x) (IntVal y) = BoolVal (op x y)
liftCompOp _ _ _ = ExnVal "Cannot lift"

--- Eval
--- ----

eval :: Exp -> Env -> Val

--- ### Constants

eval (IntExp i)  _ = IntVal i
eval (BoolExp i) _ = BoolVal i

--- ### Variables

eval (VarExp s) env = 
    case H.lookup s env of
        Just v -> v
        Nothing -> ExnVal "No match in env"

--- ### Arithmetic

eval (IntOpExp op e1 e2) env = 
    let v1 = eval e1 env
        v2 = eval e2 env
    in case op of 
        "/" -> 
            case v2 of 
                IntVal 0 -> ExnVal "Division by 0"
                _ ->
                    case H.lookup op intOps of 
                        Just f -> liftIntOp f v1 v2 
                        Nothing -> ExnVal "Cannot lift"
        _ -> 
            case H.lookup op intOps of 
                Just f -> liftIntOp f v1 v2 
                Nothing -> ExnVal "Cannot lift"


--- ### Boolean and Comparison Operators

eval (BoolOpExp op e1 e2) env =
    let v1 = eval e1 env 
        v2 = eval e2 env 
    in case H.lookup op boolOps of 
        Just f -> liftBoolOp f v1 v2 
        Nothing -> ExnVal "Cannot lift"

eval (CompOpExp op e1 e2) env = 
    let v1 = eval e1 env 
        v2 = eval e2 env 
    in case H.lookup op compOps of 
        Just f -> liftCompOp f v1 v2 
        Nothing -> ExnVal "Cannot lift"

--- ### If Expressions

eval (IfExp e1 e2 e3) env = 
    case eval e1 env of 
        BoolVal True -> eval e2 env
        BoolVal False -> eval e3 env 
        _ -> ExnVal "Condition is not a Bool"

--- ### Functions and Function Application

eval (FunExp params body) env = CloVal params body env

eval (AppExp e1 args) env = 
    let v1 = eval e1 env 
    in case v1 of 
        CloVal params body savedEnv -> 
            let argVals = map (\e -> eval e env) args 
                bindings = zip params argVals
                argEnv = H.fromList bindings
                newEnv = H.union argEnv savedEnv
            in eval body newEnv
        _ -> ExnVal "Apply to non-closure"

--- ### Let Expressions

eval (LetExp pairs body) env = 
    case pairs of 
        [] -> eval body env 
        _ -> 
            let bindings = map (\(var, val) -> (var, eval val env)) pairs
                argEnv = H.fromList bindings
                newEnv = H.union argEnv env
            in eval body newEnv

--- Statements
--- ----------

-- Statement Execution
-- -------------------

exec :: Stmt -> PEnv -> Env -> Result
exec (PrintStmt e) penv env = (val, penv, env)
    where val = show $ eval e env

--- ### Set Statements

exec (SetStmt var e) penv env =
    let v1 = eval e env
        newEnv = H.insert var v1 env
    in ("", penv, newEnv)

--- ### Sequencing

exec (SeqStmt []) penv env = ("", penv, env)
exec (SeqStmt (x:xs)) penv env = 
    let (p1, penv1, env1) = exec x penv env
        (p2, penv2, env2) = exec (SeqStmt xs) penv1 env1
    in (p1 ++ p2, penv2, env2)

--- ### If Statements

exec (IfStmt e1 s1 s2) penv env = 
    let v1 = eval e1 env 
    in case v1 of 
        BoolVal True -> exec s1 penv env
        BoolVal False -> exec s2 penv env
        _ -> ("exn: Condition is not a Bool", penv, env)

--- ### Procedure and Call Statements

exec p@(ProcedureStmt name args body) penv env = 
    let newPenv = H.insert name p penv 
    in ("", newPenv, env)

exec (CallStmt name args) penv env = 
    case H.lookup name penv of 
        Just (ProcedureStmt _ ps body) -> 
            let argVals = map (\e -> eval e env) args 
                bindings = zip ps argVals 
                paramEnv = H.fromList bindings 
                newEnv = H.union paramEnv env 
            in exec body penv newEnv
        _ -> ("Procedure f undefined", penv, env)
