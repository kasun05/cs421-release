--- Given Code
--- ==========

module Lib where

import System.IO (hPutStrLn, hPutStr, stdout, hFlush)

import Data.List (intercalate)

import Data.Functor.Identity (Identity)
import Text.ParserCombinators.Parsec hiding (Parser)
import Text.Parsec.Prim (ParsecT)

--- Metadata for autograder
--- -----------------------
tag1 = 36392
tag2 = 13977
tag3 = 68529

--- The Types
--- ---------

data Stmt = Decl String [String] Exp
            deriving (Eq)

instance Show Stmt where
    show (Decl f params exp) = f ++ " " ++ intercalate " " params ++ " = " ++ (show exp)

data Exp = IntExp Integer
         | VarExp String
         | LamExp String Exp
         | IfExp Exp Exp Exp
         | OpExp String Exp Exp
         | AppExp Exp Exp
         deriving (Eq)

instance Show Exp where
    show (VarExp s)       = s
    show (IntExp i)       = show i
    show (LamExp x e)     = "(\\" ++ x ++ " -> " ++ (show e) ++ ")"
    show (IfExp e1 e2 e3) = "(if " ++ show e1 ++ " then " ++ show e2
                            ++ " else " ++ show e3 ++ ")"
    show (OpExp op e1 e2) = "(" ++ show e1 ++ " " ++ op ++ " " ++ show e2 ++ ")"
    show (AppExp f e)     = show f ++ " " ++ show e

ctorShow :: Exp -> String
ctorShow (VarExp s)       = "VarExp " ++ show s
ctorShow (IntExp i)       = "IntExp " ++ show i
ctorShow (LamExp x e)     = "LamExp " ++ show x ++ " (" ++ ctorShow e ++ ")"
ctorShow (IfExp e1 e2 e3) = "IfExp (" ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ") ("
                                ++ ctorShow e3 ++ ")"
ctorShow (OpExp op e1 e2) = "OpExp " ++ show op ++ " ("
                                ++ ctorShow e1 ++ ") ("
                                ++ ctorShow e2 ++ ")"
ctorShow (AppExp f e)     = "AppExp (" ++ ctorShow f ++ ") (" ++ ctorShow e ++ ")"

--- Problems
--- ========

--- Manual Translation
--- ------------------

--- ### `factk :: Integer -> (Integer -> t) -> t`

factk :: Integer -> (Integer -> t) -> t
factk 0 k = k 1
factk num k = factk (num-1) (\v -> k (num*v))

--- ### `evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t`

evenoddk :: [Integer] -> (Integer -> t) -> (Integer -> t) -> t
evenoddk [x] k f = if even x then k (x) else f (x)
evenoddk (x:xs) k f = if even x then evenoddk xs (\v -> k (x+v)) f else evenoddk xs k (\v -> f (x+v))

--- Automated Translation
--- ---------------------

gensym :: Integer -> (String, Integer)
gensym i = ("v" ++ show i, i + 1)

--- ### Define `isSimple`

isSimple :: Exp -> Bool
isSimple (IntExp _) = True
isSimple (VarExp _) = True 
isSimple (LamExp _ _) = True
isSimple (IfExp e1 e2 e3) = isSimple e1 && isSimple e2 && isSimple e3
isSimple (OpExp _ e1 e2) = isSimple e1 && isSimple e2
isSimple (AppExp _ _) = False

--- ### Define `cpsExp` - Overview

cpsExp :: Exp -> Exp -> Integer -> (Exp, Integer)

--- #### Define `cpsExp` for Integer and Variable Expressions
cpsExp (IntExp i) k n = (AppExp k (IntExp i), n)
cpsExp (VarExp v) k n = (AppExp k (VarExp v), n)

--- #### Define `cpsExp` for Application Expressions
cpsExp (AppExp f e) k n = if isSimple e then 
                            (AppExp (AppExp f e) k, n)
                          else
                            let (v, n1) = gensym n 
                                cont = LamExp v (AppExp (AppExp f (VarExp v)) k)
                            in cpsExp e cont n1

--- #### Define `cpsExp` for Operator Expressions
cpsExp (OpExp op e1 e2) k n | isSimple e1 && isSimple e2 = (AppExp k (OpExp op e1 e2), n)
                            | isSimple e2 = 
                                let (v, n1) = gensym n 
                                    cont = LamExp v (AppExp k (OpExp op (VarExp v) e2))
                                in cpsExp e1 cont n1
                            | isSimple e1 =
                                let (v, n1) = gensym n 
                                    cont = LamExp v (AppExp k (OpExp op e1 (VarExp v)))
                                in cpsExp e2 cont n1
                            | otherwise = 
                                let (v1, n1) = gensym n 
                                    (v2, n2) = gensym n1
                                    cont2 = LamExp v2 (AppExp k (OpExp op (VarExp v1) (VarExp v2)))
                                    (e, i) = cpsExp e2 cont2 n2
                                    cont1 = LamExp v1 e
                                in cpsExp e1 cont1 i

--- #### Define `cpsExp` for If Expressions
cpsExp (IfExp e1 e2 e3) k n = if isSimple e1 then 
                                let (t2, n1) = cpsExp e2 k n 
                                    (t3, n2) = cpsExp e3 k n1
                                in (IfExp e1 t2 t3, n2)
                              else
                                let (v1, n1) = gensym n
                                    (t2, n2) = cpsExp e2 k n1 
                                    (t3, n3) = cpsExp e3 k n2
                                    cont = LamExp v1 (IfExp (VarExp v1) t2 t3)
                                in cpsExp e1 cont n3


--- ### Define `cpsDecl`

cpsDecl :: Stmt -> Stmt
cpsDecl (Decl name args body) = let (newbody, _) = cpsExp body (VarExp "k") 0
                     in Decl name (args ++ ["k"]) newbody
