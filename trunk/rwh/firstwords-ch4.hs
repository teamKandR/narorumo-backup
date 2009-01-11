-- Real World Haskell Exercises
-- Chapter 4: the First Word of Each Line Program
-- Alex Rudnick

-- 3) Using the command framework from the section called "A simple command
-- line framework", write a program that prints the first word of each line of
-- its input.

-- I'm going to go ahead and make it print to stdout instead. This seems
-- unnecessarily complex, but I went ahead and made it resilient to empty
-- lines.

import System.Environment (getArgs)

firstWords :: String -> [String]
firstWords text = noUncertainTerms firsts
  where
    firsts = map safeHead wordsInLines
    wordsInLines = map words lines
    lines = splitLines text

noUncertainTerms :: (Eq a) => [Maybe a] -> [a]
noUncertainTerms xs = map unwrap noNothings
  where
    unwrap (Just x) = x
    noNothings = filter (\x -> x /= Nothing) xs

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

{-- from the examples: splitLines --}
splitLines :: String -> [String]

splitLines [] = []
splitLines cs =
    let (pre, suf) = break isLineTerminator cs
    in  pre : case suf of 
                ('\r':'\n':rest) -> splitLines rest
                ('\r':rest)      -> splitLines rest
                ('\n':rest)      -> splitLines rest
                _                -> []
isLineTerminator c = c == '\r' || c == '\n'

interactWith function inputFile = do
  input <- readFile inputFile
  putStr (unlines (function input))

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input] -> interactWith function input
            _ -> putStrLn "error: exactly one argument needed"

        -- replace "id" with the name of our function below
        myFunction = firstWords
