module WogoAS where

type VarIdent = String
type TypeIdent = String

-- Logo function. --

-- maybe we don't need this yet? ...
-- data LogoDefun = LogoDefun VarIdent Expr
--   deriving (Eq, Show)

-- data Parameter = Parameters 

-- end Logo function. --

data Declaration = TypeDec TypeIdent Type | VarDec VarIdent TypeIdent Expr | FunDec VarIdent [TypedVar] TypeIdent Expr
  deriving (Eq, Show)
  
data TypedVar
  = TypedVar VarIdent TypeIdent
  deriving (Eq, Show)

data Type
  = Var TypeIdent
  | Array TypeIdent
  | Record [TypedVar]
  deriving (Eq, Show)

data Expr
  = LogoDefun VarIdent (Expr)
  | Sub Expr Expr
  | Dot Expr Expr
  | Apply VarIdent [Expr]
  | Ident TypeIdent
  | RecordVal TypeIdent [AssignField]
  | ArrayVal TypeIdent Expr Expr
  | IntLit Integer
  | StringLit String
  | While Expr Expr
  | For VarIdent Expr Expr Expr
  | If Expr Expr Expr
  | Let [Declaration] [Expr]
  | Assign Expr Expr
  | Op String Expr Expr
  | UnOp String Expr
  | Skip
  | Nil
  | Break
  | Seq [Expr]
  deriving (Show, Eq)

data AssignField 
  = AssignField VarIdent Expr
  deriving (Eq, Show)
