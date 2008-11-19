module Main where

import System
import System.IO
import Data.Char(toUpper)

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Token
import Text.ParserCombinators.Parsec

-- import Text.ParserCombinators.Parsec hiding (spaces)
-- 
-- simple :: Parser Char
-- simple = letter 
-- 
-- spaces :: Parser ()
-- spaces = skipMany1 space

-- run :: Show a => Parser a -> String -> IO ()
-- run p input
--   = case (parse p "" input) of
--       Left err -> do { putStr "parse error at "
--                   ; print err
--                   }
--       Right x  -> print x

-- readExpr :: String -> String
-- readExpr input = case parse simple "lisp" input of
--     Left err -> "No match: " ++ show err
--     Right val -> "Found value"

-- main :: IO ()
-- main = do args <- getArgs
--           putStrLn (readExpr )

-- file: ch07/toupper-lazy1.hs

-- main :: IO ()
-- main = do 
--        args <- getArgs
--        inh <- openFile (args !! 0) ReadMode
--        inpStr <- hGetContents inh
--        let result = processData inpStr
--        putStrLn result
--        putStrLn inpStr
-- 
-- processData :: String -> String
-- processData = readExpr

-- blog.moertel.com/articles/2005/08/27/power-parsing-with-haskell-and-parsec

expr    = buildExpressionParser table term
        <?> "expression"

term    =  parens expr 
        <|> natural
        <?> "simple expression"

table   = [ [prefix "-" negate, prefix "+" id ]
          , [postfix "++" (+1)]
          , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
          , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
          ]
        
binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

