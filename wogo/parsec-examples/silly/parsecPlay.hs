#!/usr/bin/env runhaskell
module Main where

import Text.ParserCombinators.Parsec
import IO
import List

simple :: Parser Char
simple = letter

{-
run :: Show a => Parser a -> String -> IO ()
run p input
        = case (parse p "" input) of
            Left err -> do{ putStr "parse error at "
                          ; print err
                          }
            Right x  -> print x
-}

leet :: Char -> Char
leet x =
    case x of
        'e' -> '3'
        't' -> '7'
        'E' -> '3'
        'T' -> '7'
        'l' -> '1'
        'L' -> '1'
        otherwise -> x

leetify :: String -> String
leetify = List.map leet

doSomething :: String -> String
doSomething = leetify
    

-- accepts a filename,
-- opens and reads the file
-- does something to the contents of the file
-- prints out the result
main :: IO ()
main = do putStrLn "Enter file name:  "
          filename     <- getLine
          input        <- readFile filename
          putStr (doSomething input)
