-- "Accordian" Patience

-- Problem statement taken from:
-- http://www.streamtech.nl/problemset/127.html

import System.Environment (getArgs)
import Data.List

type Card = (Char, Char)

playGames :: String -> String
playGames input = (show gameResults) ++ "\n"
  where
    gameResults = map runGame games
    games = map stacks joinedLines
    joinedLines = joinLines (lines input)

{- Take a line and return a list of cards. -}
stacks line = map makeStack (words line)
  where
    makeStack (r:s:[]) = [(r,s)]

{- Take a list of lines, join every second line. -}
joinLines :: [String] -> [String]
joinLines (one:two:lines) = (one ++ " " ++ two) : joinLines lines
joinLines (line:lines)
  | "#" `isPrefixOf` line = joinLines lines
  | otherwise = error "ran out of lines?"
joinLines [] = []

runGame :: [[Card]] -> [[Card]]
runGame stacks
  | stacks == stepped = stacks
  | otherwise = runGame nonEmpty
  where
    stepped = step stacks
    nonEmpty = filter (not . null) stepped

step ((l3top:l3s):l2s:l1s:(l0top:l0s):stacks)
  | l0top `matches` l3top = (l0top:l3top:l3s):l2s:l1s:l0s:stacks

{-
step ((l1top:l1s):(l0top:l0s):stacks)
  | l0top `matches` l1top = (l0top:l1top:l1s):l0s:stacks
-}

step (s:stacks) = s : step stacks
step [] = []

matches (r1,s1) (r2,s2) = r1 == r2 || s1 == s2

interactWith function inputFile = do
  input <- readFile inputFile
  putStr (function input)

main = do
        args <- getArgs
        case args of
          [input] -> interactWith playGames input
          _ -> putStrLn "error: exactly one argument needed"
