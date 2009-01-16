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
playGames input = (show games) ++ "\n"
  where
    gameResults = map runGame games
    games = map toStacks joinedLines
    joinedLines = joinLines (lines input)

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
        case args of
          [inputFile] -> do
                       input <- readFile inputFile
                       putStr (playGames input)
          _ -> putStrLn "error: exactly one argument needed"
