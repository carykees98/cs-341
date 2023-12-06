import qualified TestQ5 as TestQ5

{-
The main function reads in the program from a file
  then returns the memory after the program runs
-} 
main = do
  putStrLn "What is your input file?"
  inName <- getLine
  contents <- readFile inName 
  let output = TestQ5.run contents
  print output 
  
