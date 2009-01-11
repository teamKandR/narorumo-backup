-- Real World Haskell Exercises
-- Chapter 4: Transpose the text!
-- Alex Rudnick

-- 4) Write a program that transposes the text in a file. For instance, it
-- should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

-- I'm going to go ahead and make it print to stdout instead.

import System.Environment (getArgs)
import Data.List

transposeText :: String -> String
transposeText = unlines . transpose . lines

interactWith function inputFile = do
  input <- readFile inputFile
  putStr (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> interactWith function input
            _ -> putStrLn "error: exactly one argument needed"

        -- replace "id" with the name of our function below
        myFunction = transposeText
