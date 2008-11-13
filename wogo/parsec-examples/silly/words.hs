#!/usr/bin/env runhaskell
module Main where

import Text.ParserCombinators.Parsec
import IO
import List


parens :: Parser () 
parens = do{ char '(' 
           ; parens 
           ; char ')' 
           ; parens 
           } 
        <|> return () 


nesting :: Parser Int 
nesting = do{ char '(' 
            ; n <- nesting 
            ; char ')' 
            ; m <- nesting 
            ; return (max (n+1) m) 
            }
            <|> return 0 

simple :: Parser Char
simple = letter

wschar :: Parser Char
wschar = space <|> char '\n' <|> char '\t' <|> char '\r'

ws :: Parser ()
ws = skipMany1 wschar <?> "whitespace"

word :: Parser String
word = many1 letter <?> "identifier"

sentence :: Parser [String]
sentence = do
            words <- sepBy1 word ws
            punct <- oneOf ".?!" <?> "punctuation"
            return (words++[[punct]])
             
-- run sentence "The cat ran to   the   ball!"
  -- it ignores trailing jibberish
-- run sentence "This is sane.!333434"


run :: Show a => Parser a -> String -> IO () 
run p input =
    case (parse p "" input) of 
        Left err ->
            do putStr "parse error at " 
               print err  
        Right x -> print x

main :: IO ()
main = run sentence "This  somehow  is    sane."
