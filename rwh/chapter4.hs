-- Real World Haskell Exercises
-- Chapter 4
-- Alex Rudnick

import Data.List hiding (intersperse)
import Data.Char

-- 1) Write your own "safe" definitions of the standard partial list
-- functions, but make sure that yours never fail.

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:tl) = Just tl

safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast (x:[]) = Just x
safeLast (_:xs) = safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] = Nothing
safeInit (x:[]) = Just []
safeInit (x:xs) = Just (x : unpacked)
  where
    unpacked = unpack (safeInit (xs))
    unpack (Just safe) = safe

-- 2) Write a function splitWith that acts similarly to words, but takes a
-- predicate and a list of any type, and splits its input list on every element
-- for which the predicate returns False.

splitWith :: (a -> Bool) -> [a] -> [[a]]

splitWith _ [] = []

splitWith pred (x:xs) 
  | pred x = splitWith pred xs

splitWith pred xs = thischunk : otherchunks
  where
    notpred = not . pred
    thischunk = takeWhile notpred xs
    otherchunks = splitWith pred (dropWhile notpred xs)

-- 3) Using the command framework from the section called "A simple command
-- line framework", write a program that prints the first word of each line of
-- its input.

-- OK. See firstwords-ch4.hs for complete program.
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

-- 4) Write a program that transposes the text in a file. For instance, it
-- should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

transposeText :: String -> String
transposeText = unlines . transpose . lines

-- OK. See transpose-text-ch4.hs for complete program.

-- Other 1) Use a fold (choosing the appropriate fold will make your code much
-- simpler) to rewrite and improve upon the asInt function from the section
-- called "Explicit recursion" Extend your function to handle the following
-- kinds of exceptional conditions by calling error.

asInt_fold :: String -> Int

asInt_fold ('-':str) = negate (asInt_fold str)
asInt_fold str = foldl addplace 0 str
  where
    addplace left right = (10 * left) + (digitToInt right)

-- Other 2) (let's do this one later.)

-- Other 3) The Prelude function concat concatenates a list of lists into a
-- single list, and has the following type. Write your own definition of concat
-- using foldr.

myconcat :: [[a]] -> [a]
myconcat lsts = foldr (++) [] lsts

-- Other 4) Write your own definition of the standard takeWhile function, first
-- using explicit recursion, then foldr.

takeWhileRecursive pred (x:xs)
  | (pred x) = x : (takeWhileRecursive pred xs)
  | otherwise = []

takeWhileFold pred xs = foldr maybeCons [] xs
  where
    maybeCons x xs
      | (pred x) = x : xs
      | otherwise = []

-- 5) The Data.List module defines a function, groupBy, which has the following
-- type. Use ghci to load the Data.List module and figure out what groupBy
-- does, then write your own implementation using a fold.

-- Break things up into sublists, such that the predicate holds true for the
-- first element and each subsequent element of the sublist. So for the
-- predicate (>), each sublist starts with a number where every other number in
-- that group is lower than it.

mygroupby :: (a -> a -> Bool) -> [a] -> [[a]]
mygroupby pred lst = foldl handleNext [] lst
  where
    handleNext lsts right
      | null lsts = [[right]]
      | (pred leader right) = (init lsts) ++ [ (last lsts) ++ [right] ]
      | otherwise = lsts ++ [[right]]
      where
        group = (last lsts)
        leader = head group

-- 6) How many of the following Prelude functions can you rewrite using list
-- folds? (A: all of them!)
any :: (a -> Bool) -> [a] -> Bool
any pred lst = foldr (\left right -> (pred left) || right) False lst

cycle :: [a] -> [a]
cycle lst = foldr (:) lst lst

mywords :: String -> [String]
mywords str = noEmpty (foldr breakws [] str)
  where
    breakws c (w:ws)
      | (isSpace c) && w == "" = w:ws
      | isSpace c = "":(w:ws)
      | otherwise = (c:w):ws
    breakws c [] = [[c]]
    noEmpty ("":words) = words
    noEmpty words = words

myunlines :: [String] -> String
myunlines strs = foldr appendNL "" strs
  where
    appendNL left right = left ++ "\n" ++ right
