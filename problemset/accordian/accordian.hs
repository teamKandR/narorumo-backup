-- "Accordian" Patience

-- Problem statement taken from:
-- http://www.streamtech.nl/problemset/127.html

import System.Environment (getArgs)
import Data.List

type Card = [Char]
type Stack = [Card]

runGame :: [Stack] -> [Stack]
runGame stacks
  | stacks == stepped = stacks
  | otherwise = runGame stepped
  where
    stepped = step stacks

-- a, b, c, d are the leftmost 4 stacks.
step ((a:as):(b:bs):(c:cs):(d:ds):stacks)
  | b `matches` a = nonEmpty ((b:a:as):bs:(c:cs):(d:ds):stacks)
  | c `matches` b = nonEmpty ((a:as):(c:b:bs):cs:(d:ds):stacks)
  | d `matches` a = nonEmpty ((d:a:as):(b:bs):(c:cs):ds:stacks)

step ((a:as):(b:bs):stacks)
  | a `matches` b = nonEmpty ((b:a:as):bs:stacks)

step (s:stacks) = s : step stacks
step [] = []

matches (r1:s1:[]) (r2:s2:[]) = r1 == r2 || s1 == s2
nonEmpty = filter (not . null)

playGames :: String -> String
playGames input = unlines shownResults
  where
    shownResults = map showResults gameResults
    gameResults = map runGame games
    games = map toStacks joinedLines
    joinedLines = joinLines (lines input)

showResults stacks
  | length stacks == 1 = "1 pile remaining: 52"
  | otherwise = (shownNumStacks) ++ " piles remaining: " ++ stackCounts
  where
    shownNumStacks = show (length stacks)
    stackCounts = unwords (map (show . length) stacks)

{- Take a line and return a list of card stacks. -}
toStacks line = map makeStack (words line)
  where
    makeStack (r:s:[]) = [r:s:[]]

{- Take a list of lines, join every second line. -}
joinLines :: [String] -> [String]
joinLines (one:two:lines) = (one ++ " " ++ two) : joinLines lines
joinLines (line:lines)
  | "#" `isPrefixOf` line = joinLines lines
  | otherwise = error "ran out of lines?"
joinLines [] = []

main = do
        args <- getArgs
        input <- case args of
                  [inputFile] -> (readFile inputFile)
                  _ -> readFile "input.txt"
        putStr (playGames input)
