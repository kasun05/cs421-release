--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Algebraic Data Types
--- --------------------

data List a = Cons a (List a)
            | Nil
  deriving (Show, Eq)

data Exp = IntExp Integer
         | PlusExp [Exp]
         | MultExp [Exp]
  deriving (Show, Eq)

--- ### list2cons

-- don't forget to put the type declaration or you will lose points!
list2cons :: [a] -> List a
list2cons [] = Nil
list2cons (x:xs) = Cons x (list2cons xs)

--- ### cons2list

-- don't forget to put the type declaration or you will lose points!
cons2list :: List a -> [a]
cons2list Nil = []
cons2list (Cons i l) = i : cons2list l

--- ### eval

-- don't forget to put the type declaration or you will lose points!
eval :: Exp -> Integer
eval (IntExp n) = n
eval (PlusExp es) = foldr (+) 0 (map eval es)
eval (MultExp es) = foldr (*) 1 (map eval es)

--- ### list2cons'

-- don't forget to put the type declaration or you will lose points!
list2cons' :: [a] -> List a
list2cons' x = foldr Cons Nil x

--- ### BinTree

-- BinTree
data BinTree a = Node a (BinTree a) (BinTree a)
               | Leaf
  deriving (Show)

--- ### sumTree

-- don't forget to put the type declaration or you will lose points!
sumTree :: Num a => BinTree a -> a
sumTree Leaf = 0
sumTree (Node a b c) = a + sumTree (b) + sumTree (c)

--- ### SimpVal

-- SimpVal
data SimpVal = IntVal Integer
             | BoolVal Bool
             | StrVal String
             | ExnVal String
  deriving (Show)

--- ### liftIntOp

-- don't forget to put the type declaration or you will lose points!
liftIntOp :: (Integer -> Integer -> Integer) -> SimpVal -> SimpVal -> SimpVal
liftIntOp x (IntVal y) (IntVal z) = IntVal (x y z)
liftIntOp x _ _ = ExnVal "not an IntVal!"
