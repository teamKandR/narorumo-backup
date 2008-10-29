module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System
import System.IO
import Data.Char(toUpper)

simple :: Parser Char
simple = letter 

spaces :: Parser ()
spaces = skipMany1 space

run :: Show a => Parser a -> String -> IO ()
run p input
  = case (parse p "" input) of
      Left err -> do { putStr "parse error at "
                  ; print err
                  }
      Right x  -> print x

readExpr :: String -> String
readExpr input = case parse simple "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value"

-- main :: IO ()
-- main = do args <- getArgs
--           putStrLn (readExpr )

-- file: ch07/toupper-lazy1.hs

main :: IO ()
main = do 
       args <- getArgs
       inh <- openFile (args !! 0) ReadMode
       inpStr <- hGetContents inh
       let result = processData inpStr
       putStrLn result
       putStrLn inpStr

processData :: String -> String
processData = readExpr
