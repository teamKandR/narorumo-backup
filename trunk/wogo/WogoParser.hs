-- Parser for Wogo, now based on the Tiger parser from the Parsec examples.

module WogoParser where

import WogoAS
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language( emptyDef )
-- import Text.ParserCombinators.Parsec.Language( javaStyle )

prettyLogoFromFile fname
  = do{ input <- readFile fname
      ; putStr input
      ; case parse program fname input of
           Left err -> do{ putStr "parse error at "
                           ; print err
                           }
           Right x  -> print x
      }

toExpr :: Parser Expr
toExpr = do{ reserved "to"
            ; p <- parameter -- this will be multiple parameters next.
            ; e <- expr
            ; reserved "end"
            ; return (LogoDefun p e)
            }

-- parameters :: Parser Parameters
-- parameters = many parameter

parameter = P.identifier lexer    

-----------------------------------------------------------
-- A program is many expressions. Actually, logo makes a distinction between
-- expressions and statements, doesn't it? ...
-----------------------------------------------------------
program
    = do{ whiteSpace
        ; e <- exprs
        ; return e
        }

----------------------------------------------------------------
-- All types of expression(s)
----------------------------------------------------------------

exprs = many expr

expr :: Parser Expr
expr = choice 
       [ do{ reserved "break"
           ; return Break
           }
       , toExpr
       , ifExpr
       , whileExpr
       , forExpr
       , sequenceExpr       
       ]

recordExpr :: Parser Expr
recordExpr = do{ tid <- identifier
               ; symbol "{"
               ; fields <- commaSep1 fieldAssign
               ; symbol "}"
               ; return (RecordVal tid fields)
               }

fieldAssign :: Parser AssignField
fieldAssign = do{ id <- identifier
                ; symbol "="
                ; e <- expr
                ; return (AssignField id e)
                }
               
arrayExpr :: Parser Expr
arrayExpr = do{ tid <- identifier
              ; size <- brackets expr
              ; reserved "of"
              ; initvalue <- expr
              ; return (ArrayVal tid size initvalue)
              }

ifExpr :: Parser Expr
ifExpr = do{ reserved "if"
             ; cond <- expr
             ; reserved "then"
             ; thenpart <- expr
             ; elsepart <- option Skip (do{ reserved "else"; expr})
             ; return (If cond thenpart elsepart)
             }
             
whileExpr :: Parser Expr
whileExpr = do{ reserved "while"
              ; cond <- expr
              ; reserved "do"
              ; body <- expr
              ; return (While cond body)
              }

forExpr :: Parser Expr
forExpr = do{ reserved "for"
            ; id <- identifier
            ; symbol ":="
            ; lowerbound <- expr
            ; reserved "to"
            ; upperbound <- expr
            ; reserved "do"
            ; body <- expr
            ; return (For id lowerbound upperbound body)
            }
           
sequenceExpr :: Parser Expr
sequenceExpr = do{ exps <- parens (semiSep1 expr)
                 ; return (if length exps < 2 then head exps else Seq exps)
                 }

operators =
    [ [ prefix "-"]
    , [ op "*"  AssocLeft, op "/"  AssocLeft ]
    , [ op "+"  AssocLeft, op "-"  AssocLeft ]
    , [ op "=" AssocNone, op "<>" AssocNone, op "<="  AssocNone
      , op "<" AssocNone, op ">="  AssocNone, op ">" AssocNone ]
    , [ op "&" AssocRight ] -- Right for shortcircuiting
    , [ op "|" AssocRight ] -- Right for shortcircuiting
    , [ op ":=" AssocRight ]
    ]
    where
      op name assoc   = Infix (do{ reservedOp name
                                  ; return (\x y -> Op name x y) 
                                  }) assoc
      prefix name     = Prefix  (do{ reservedOp name
                                  ; return (\x -> UnOp name x)
                                  })                                  

funCallExpr = do{ id <- identifier
                 ; parms <- parens (commaSep expr)
                 ; return (Apply id parms)
                 }

intLiteral = do{ i <- integer; return (IntLit i) }
strLiteral = do{ s <- stringLiteral; return (StringLit s) }
variable = do{ id <- identifier
             ; return (Ident id)
             }
             

-----------------------------------------------------------
-- The lexer
-----------------------------------------------------------
lexer     = P.makeTokenParser tigerDef

tigerDef  = emptyDef
          { -- Kept the Java single line comments, but officially the language has no comments
            P.identStart = letter <|> char ':'
          , P.reservedNames  = [ "array", "break", "do", "else", "end", "for", "function", 
                                 "if", "in", "let", 
                                 "nil", "of", "then", "to", "type", "var", "while" ]
          , P.reservedOpNames= [ "<", "<=", ">", ">=", ":=", "+", "&", "-", "/"]
          , P.opLetter       = oneOf (concat (P.reservedOpNames tigerDef))
          , P.caseSensitive  = True   
          }

parens          = P.parens lexer    
braces          = P.braces lexer    
semiSep         = P.semiSep lexer  
semiSep1        = P.semiSep1 lexer    
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer    
symbol          = P.symbol lexer    
identifier      = P.identifier lexer    
reserved        = P.reserved lexer    
reservedOp      = P.reservedOp lexer
integer         = P.integer lexer    
charLiteral     = P.charLiteral lexer    
stringLiteral   = P.stringLiteral lexer    
