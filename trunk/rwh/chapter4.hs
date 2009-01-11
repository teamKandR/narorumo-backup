-- Real World Haskell Exercises
-- Chapter 4
-- Alex Rudnick

import Data.List hiding (intersperse)

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

