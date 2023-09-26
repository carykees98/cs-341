-- Completed by Cary Keesler

module Hw1 (Memory, allValues, declare, getValue, modify, putValue, swap, incrementAll, rmVar, rmPos, allPos) where

-- Memory is a type synonym
-- it says a Memory is a list of pairs of String and Int
-- the String is the variable name
-- the Int is the value of the variable
type Memory = [(String, Int)]

-- example memory
mem :: Memory
mem = [("x", 10), ("y", 20), ("z", 5)]

-- 1. allValues takes a Memory and the name of a variable
-- it returns a list of all values that variable has in Memory
-- Note: in real memory, a variable should not appear more than once
-- but if it does the list will have more than one element
-- Hint: Use list comprehension
-- Example 1:

-- * Main> allValues mem "y"

-- [20]
-- Example 2:

-- * Main> allValues mem "w"

-- []
allValues :: Memory -> String -> [Int]
-- Replace the following line with your solution
allValues ps v = [snd x | x <- ps, fst x == v]

-- 2. getValue takes a Memory and the name of a variable
-- if the variable does not exist it returns an exception: Not Defined
-- if the variable has more than one value it returns an exception: Multiple Values
-- if the variable has only one value it returns its value
-- Hint: call allValues, use if then else, use a where clause
-- Example 1:

-- * Main> getValue mem "y"

-- 20
-- Example 2:

-- * Main> getValue mem "w"

-- *** Exception: Not Defined

-- CallStack (from HasCallStack):
--   error, called at hw1.hs:26:35 in main:Main
getValue :: Memory -> String -> Int
-- Replace the following line with your solution
getValue ps v
  | null valueList = error "Not Defined"
  | length valueList == 1 = head valueList
  | otherwise = error "Multiple Values"
  where
    valueList = allValues ps v

-- 3. declare takes Memory and the name of a variable var
-- it adds var to memory with value 0
-- it returns the updated memory
-- assume that var does not already exist in memory
-- Example:

-- * Main> declare mem "count"

-- [("count",0),("x",10),("y",20),("z",5)]
declare :: Memory -> String -> Memory
-- Replace the following line with your solution
declare ps var = (var, 0) : ps

-- 4. modify takes 2 variable names x an y, and two values val1 and val2
-- if x and y are the same variable return val1 else return val2
-- Hint: use if then else
-- Example 1:

-- * Main> modify "a" "a" 1 2

-- 1
-- Example 2:

-- * Main> modify "a" "b" 1 2

-- 2
modify :: String -> String -> Int -> Int -> Int
-- Replace the following line with your solution
modify s1 s2 val1 val2 = if s1 == s2 then val1 else val2

-- 5. putValue takes a Memory, a variable name var and a value val
-- it changes the memory, so that var now has the value val
-- it returns the updated memory
-- you can assume x is already in the memory
-- Hint: use list comprehension and call modify
-- Example:

-- * Main> putValue mem "y" 40

-- [("x",10),("y",40),("z",5)]
putValue :: Memory -> String -> Int -> Memory
-- Replace the following line with your solution
putValue ps var val = [(x, y) | (x, z) <- ps, y <- [modify var x val z]]

-- 6. swap takes a Memory and two variable names x and y
-- it swaps the value of x and y and returns the updated memory
-- Note: Use putValue and two where clauses
-- Example:

-- * Main> swap mem "x" "z"

-- [("x",5),("y",20),("z",10)]
swap :: Memory -> String -> String -> Memory
-- Replace the following line with your solution
swap ps x y =
  let mem1 = putValue mem x (getValue mem y)
   in let newMem = putValue mem1 y (getValue mem x)
       in newMem

-- 7. incrementAll takes a Memory
-- it adds one to all variables in memory and returns the updated memory
-- Hint: Use list comprehension
-- Example:

-- * Main> incrementAll mem

-- [("x",11),("y",21),("z",6)]
incrementAll :: Memory -> Memory
-- Replace the following line with your solution
incrementAll ps = [(x, y + 1) | (x, y) <- ps]

-- 8.rmVar takes a Memory and a variable name
-- it returns that variable from memory and returns the updated memory
-- Hint: Use list comprehension
-- Example:

-- * Main> rmVar mem "y"

-- [("x",10),("z",5)]
rmVar :: Memory -> String -> Memory
-- Replace the following line with your solution
rmVar ps v = [(x, y) | (x, y) <- ps, x /= v]

-- 9. rmPos takes a Memory and removes all variables that > 0
-- it returns the updated memory
-- Hint: Use list comprehension
-- Example:

-- * Main> rmPos mem

-- []
rmPos :: Memory -> Memory
-- Replace the following line with your solution
rmPos ps = [(x, y) | (x, y) <- ps, y <= 0]

-- 10. allPos takes a Memory and returns True if all variables are > 0
-- Hint: Call rmPos
-- Example:

-- * Main> allPos mem

-- True
allPos :: Memory -> Bool
-- Replace the following line with your solution
allPos ps = null (rmPos mem)
