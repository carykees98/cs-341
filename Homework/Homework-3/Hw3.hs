-- Completed by Cary Keesler

module Hw2 (getValue, putValue, twice, clear, posMem, maxVal, execInstr, exec, putValuef, twicef, execInstrf, execf) where

import Debug.Trace

-- Memory is a list of pairs of variable names and values
type Memory = [(String, Int)]

mem1 :: Memory
mem1 = [("x", 1), ("y", -2), ("z", 3)]

-- Recursion and list comprehension are not allowed in this assignment

{-
getValue takes 2 arguments
  1. The name of a variable x
  2. A Memory, as in the previous assignment
getValue returns the value of x
Assume variable x occurs exactly once in memory
Example:
getValue "z" mem1
3
-}
getValue :: String -> Memory -> Int
-- Fill in your code here
getValue x mem = snd $ head $ filter (\(z, _) -> z == x) mem

{-
putValue takes 3 arguments
  1. The name of a variable x
  2. An Int m
  3. A Memory, as in the previous assignment
putValue should assign x to m and return the updated memory
Assume x occurs exactly once in the memory
Example:
putValue "y" 4 mem1
[("x",1),("y",4),("z",3)]
-}
putValue :: String -> Int -> Memory -> Memory
-- Fill in your code here
putValue x n mem = map (\(z, y) -> if z == x then (z, n) else (z, y)) mem

{-
twice takes a memory as argument
and returns that memory with the value of every variable doubled
Example:
twice mem1
[("x",2),("y",-4),("z",6)]
-}
twice :: Memory -> Memory
-- Fill in your code here
twice mem = map (\(x, y) -> (x, 2 * y)) mem

{- clear takes a memory as argument
and returns that memory with the value of every variable set to zero
Example:
clear mem1
[("x",0),("y",0),("z",0)]
-}
clear :: Memory -> Memory
-- Fill in your code here
clear mem = map (\(x, y) -> (x, 0)) mem

{-
posMem takes a memory as argument
and returns a list of all variables greater than or equal to zero
Example:
posMem mem1
["x","z"]
-}
posMem :: Memory -> [String]
-- Fill in your code here
posMem mem = map fst (filter (\(_, y) -> y >= 0) mem)

{-
maxVal takes a memory as argument
and returns a the variable with the largest value
assume the memory is not empty
maxVal mem1
"z"
-}
maxVal :: Memory -> String
-- Fill in your code here
maxVal mem = fst $ foldl1 (\x y -> if snd x > snd y then x else y) mem

{-
An instruction is a triple containing
1. The name of the instruction
2. The variable that is involved
3. The value of the variable
-}
type Inst = (String, String, Int)

-- A program is a list of instructions
type Program = [Inst]

{-
There are three kinds of instructions
1. ("declare",x,n) creates a variable x and sets it equal to n
      it assumes that x does not currently exist
2. ("load",x,n) sets an existing variable x to n
    it assumes that x already exists exactly once
3. ("add",x,n) adds n to the value of x and stores it back in x
-}
prog1 :: Program
prog1 = [("declare", "x", 10), ("declare", "y", 20), ("load", "x", 40), ("add", "y", 80)]

{-
execInstr has 2 arguments
1. a memory
2. an instruction
It executes that instruction on that memory and returns the resulting memory
-}
execInstr :: Memory -> Inst -> Memory
-- fill in your code here
execInstr mem (x, y, z)
  | x == "declare" = (y, z) : mem
  | x == "load" = putValue y z mem
  | x == "add" = putValue y (getValue y mem + z) mem

{-
exec has 2 arguments
1. the name of a variable x
2. A program
exec runs the program and returns the value of x at the end
Examples:
exec "x"prog1
40
exec "y"prog1
100
-}
exec :: String -> Program -> Int
-- Fill in your code here
exec x p = getValue x $ foldl execInstr [] p

{-
Now we consider a different way to represent memory
Memoryf is a function from variable names to values
-}
type Memoryf = String -> Int

{-
memf1 gives all variables the value of zero
The following putValue commands modify the function
Example:
memf4 "z"
3
-}
memf1 = \_ -> 0

memf2 = putValuef "z" 3 memf1

memf3 = putValuef "y" (-2) memf2

memf4 = putValuef "x" 1 memf3

{-
putValue takes 3 arguments
  1. The name of a variable x
  2. An Int m
  3. A Memoryf function
putValue should assign x to m and return the updated Memoryf function
This function will work even if x has not been previously assigned
Example:
(putValuef "x" 5 memf4) "x"
5
(putValuef "w" 5 memf4) "w"
5
-}
putValuef :: String -> Int -> Memoryf -> Memoryf
-- Fill in your code here
putValuef x m memf = \y -> if y == x then m else memf y

{-
twicef takes a Memoryf and doubles the value of everthing in the memory
It returns back an updated Memoryf function
Example:
twicef memf4 "z"
6
-}
twicef :: Memoryf -> Memoryf
-- Fill in your code here
twicef memf = \y -> 2 * memf y

{-
execInstrf has 2 arguments
1. a Memoryf function
2. an instruction
It executes that instruction on that Memoryf and returns the resulting Memoryf
The only instructions are load and add
  there is no declare this time
-}
execInstrf :: Memoryf -> Inst -> Memoryf
-- Fill in your code here
execInstrf memf (x, y, z)
  | x == "load" = putValuef y z memf
  | x == "add" = putValuef y (memf y + z) memf

prog2 :: Program
prog2 = [("load", "y", 20), ("load", "x", 40), ("add", "y", 80)]

{-
execf has 2 arguments
1. the name of a variable x
2. A program
execf runs the program and returns the value of x at the end
There is no declare statements
  it must initialize everything to zero at the beginning of the program
Examples:
execf "x"prog2
40
execf "y"prog2
100
-}
execf :: String -> Program -> Int
execf x p = foldl execInstrf memf1 p x
