import Text.ParserCombinators.ReadP
import Data.Char

-- checks if there is a single parse which uses entire string
-- and returns the result of that parse
parse :: ReadP a -> String -> a
parse p s   
  | null parses        = error "There are no parses"
  | length parses > 1  = error "There is more than one parse"
  | otherwise          = head parses  
    where parses = [x | (x,"") <- readP_to_S p s]
   
-- removes white space and then parses
parsews :: ReadP a -> String -> a
parsews p s = parse p [c | c <- s, not (isSpace c)]

-- store an expression
data Exp = Num Int | Var String | Plus Exp Exp
         | Minus Exp Exp | Times Exp Exp | Divide Exp Exp
  deriving Show

-- variables and their values
type Memory = [(String,Int)]

-- parse an expression and evaluate it in the memory
run :: String -> Memory -> Int
run s m = eval (parsews expr s) m

-- evaluate an expression
eval :: Exp -> Memory -> Int
eval (Num n) _ = n
eval (Plus e1 e2) mem = eval e1 mem + eval e2 mem
eval (Minus e1 e2) mem = eval e1 mem - eval e2 mem
eval (Times e1 e2) mem = eval e1 mem * eval e2 mem
eval (Divide e1 e2) mem = eval e1 mem `div` eval e2 mem
eval (Var v) mem
  | answer == Nothing    = error (v ++ " is not assigned")
  | otherwise            = val  
    where answer = lookup v mem
          Just val = answer

-- parse a chain of factors, separated by + or - symbol
-- store as a single expression
expr :: ReadP Exp
expr = chainl1 factor plus

-- read a + or - symbol
-- combine expressions on lhs and rhs into single expression 
plus :: ReadP (Exp -> Exp -> Exp)
plus = do 
  op <- char '+' <++ char '-'
  if op == '+' then return Plus else return Minus

-- parse a chain of basic expressions, separated by * or / symbol
-- store as a single expression
factor :: ReadP Exp
factor = chainl1 base times

-- read a * or / symbol
-- combine expressions on lhs and rhs into single expression 
times :: ReadP (Exp -> Exp -> Exp)
times = do
  op <- char '*' <++ char '/'
  if op == '*' then return Times else return Divide

-- parse a basic expression
-- it will be a parenthesized expression, a number or a variable
base :: ReadP Exp
base = parens <++ num <++ var

-- parse a parenthesized expresssion
parens :: ReadP Exp
parens = do  
  char '('
  e <- expr
  char ')'
  return e

-- parse a number
num :: ReadP Exp
num = do
  x <- munch1 isDigit
  let n = read x :: Int
  return (Num n)

-- parse a variable
var :: ReadP Exp
var = do
  s <- munch1 isAlpha
  return (Var s)

-- parse a fully parenthesized expression
-- it will be a complex expression, a number or a variable
-- if you know that your expression is fully parenthesized, this is a simpler parser than the above
-- given the above, you don't need this function
-- I just included it here as an example of a simpler parser
expp :: ReadP Exp
expp = complexp <++ var <++ num

-- parse a fully parenthesized complext expression
-- only necessary expp function
complexp :: ReadP Exp
complexp = do 
  char '('
  e1 <- expp 
  op <- char '+' <++ char '-' <++ char '*' <++ char '/'
  e2 <- expp
  char ')'
  return $ if op == '+' then Plus e1 e2
           else if op == '-' then Minus e1 e2
           else if op == '*' then Times e1 e2
           else Divide e1 e2

