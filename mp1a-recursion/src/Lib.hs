--- Getting Started
--- ===============

--- Relevant Files
--- --------------

module Lib where

-- This line imports the Prelude module without certain functions
import Prelude hiding ( take, drop, reverse
                      , zip, zipWith
                      , map, foldl, foldr
                      , iterate, repeat
                      , replicate, cycle
                      , (++)
                      )
-- When you are allowed to use builtin functions Prepend them with "P."
-- for example `P.take`
import qualified Prelude as P

--- Metadata for autograder
--- -----------------------
tag1 = 21923
tag2 = 44437
tag3 = 24929

--- Problems
--- ========

--- Recursion
--- ---------

--- ### mytake

-- don't forget to put the type declaration or you will lose points!
mytake :: Int -> [a] -> [a]
mytake _ [] = []
mytake n (x:xs) | n <= 0 = []
                | otherwise = x : mytake (n-1) xs

--- ### mydrop

-- don't forget to put the type declaration or you will lose points!
mydrop :: Int -> [a] -> [a]
mydrop _ [] = []
mydrop n xs | n <= 0 = xs
mydrop n (x:xs) = mydrop (n-1) xs

--- ### rev

-- don't forget to put the type declaration or you will lose points!
rev :: [a] -> [a]
rev xs = aux [] xs
    where aux left [] = left
          aux left (x:xs) = aux (x : left) xs

--- ### app

-- don't forget to put the type declaration or you will lose points!
app :: [a] -> [a] -> [a]
app [] y = y
app (x:xs) y = x : app xs y

--- ### inclist

-- don't forget to put the type declaration or you will lose points!
inclist :: Num a => [a] -> [a]
inclist [] = []
inclist (x:xs) = (x+1) : inclist xs

--- ### sumlist

-- don't forget to put the type declaration or you will lose points!
sumlist :: Num a => [a] -> a
sumlist x = aux 0 x
    where aux n [] = n
          aux n (x:xs) = aux (n+x) xs

--- ### myzip

-- don't forget to put the type declaration or you will lose points!
myzip :: [a] -> [b] -> [(a,b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x,y) : myzip xs ys

--- ### addpairs

-- don't forget to put the type declaration or you will lose points!
addpairs :: (Num a) => [a] -> [a] -> [a]
addpairs x y = add (myzip x y)
    where add [] = []
          add ((x,y):xs) = (x+y) : add xs

--- ### ones

-- don't forget to put the type declaration or you will lose points!
ones :: [Integer]
ones = 1 : ones

--- ### nats

-- don't forget to put the type declaration or you will lose points!
nats :: [Integer]
nats = aux 0
    where aux n = n : aux (n+1)

--- ### fib

-- don't forget to put the type declaration or you will lose points!
fib :: [Integer]
fib = 0 : 1 : addpairs fib (tail fib)

--- Set Theory
--- ----------

--- ### add

-- don't forget to put the type declaration or you will lose points!
add :: Ord a => a -> [a] -> [a]
add x [] = [x]
add x (y:ys) | x == y = y : ys
             | x < y = x : y : ys
             | otherwise = y : add x ys

--- ### union

-- don't forget to put the type declaration or you will lose points!
union :: Ord a => [a] -> [a] -> [a]
union [] y = y
union x [] = x
union (x:xs) (y:ys) | x == y = x : union xs ys
                    | x > y = y : union (x:xs) ys
                    | otherwise = x : union xs (y:ys)

--- ### intersect

-- don't forget to put the type declaration or you will lose points!
intersect :: Ord a => [a] -> [a] -> [a]
intersect [] y = []
intersect x [] = []
intersect (x:xs) (y:ys) | x > y = intersect (x:xs) ys
                        | x < y = intersect xs (y:ys)
                        | otherwise = x : intersect xs ys

--- ### powerset

-- don't forget to put the type declaration or you will lose points!
powerset :: Ord a => [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = 
    let ps = powerset xs
    in union ps (combine ps)
    where 
        combine [] = []
        combine (y:ys) = add x y : combine ys

--- Higher Order Functions
--- ----------------------

--- ### inclist'

-- don't forget to put the type declaration or you will lose points!
inclist' :: Num a => [a] -> [a]
inclist' = P.map (+1)

--- ### sumlist'

-- don't forget to put the type declaration or you will lose points!
sumlist' :: (Num a) => [a] -> a
sumlist' = P.foldr (+) 0
