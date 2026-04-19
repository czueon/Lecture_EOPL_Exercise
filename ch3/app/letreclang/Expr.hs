{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expr where

type Program = Exp
  
data Exp =
  Const_Exp Int               -- Expression -> integer_number
  | Diff_Exp Exp Exp            -- Expression -> - integer_number
                                -- Expression -> - (Expression, Expression)
  | IsZero_Exp Exp                -- Expression -> zero? (Expression)
  | If_Exp Exp Exp Exp          -- Expression -> if Expression then Expression else Expression
  | Var_Exp Identifier          -- Expression -> identifier
  | Let_Exp Identifier Exp Exp  -- Expression -> let identifier = Expression in Expression
  | Proc_Exp Identifier Exp       -- Expression -> proc (identifier) Expression
  | Call_Exp Exp Exp              -- Expression -> (Expression Expression)
  | Letrec_Exp Identifier Identifier Exp Exp -- Expression -> letrec identifier (identifier) = Expression in Expression
  deriving Show

type Identifier = String

const_exp :: Int -> Exp
const_exp n = Const_Exp n

diff_exp :: Exp -> Exp -> Exp
diff_exp e1 e2 = Diff_Exp e1 e2

iszero_exp :: Exp -> Exp
iszero_exp e = IsZero_Exp e

if_exp :: Exp -> Exp -> Exp -> Exp
if_exp e1 e2 e3 = If_Exp e1 e2 e3

var_exp :: Identifier -> Exp
var_exp s = Var_Exp s

let_exp :: Identifier -> Exp -> Exp -> Exp
let_exp x e1 e2 = Let_Exp x e1 e2

letrec_exp :: Identifier -> Identifier -> Exp -> Exp -> Exp
letrec_exp f x e1 e2 = Letrec_Exp f x e1 e2

proc_exp :: Identifier -> Exp -> Exp
proc_exp x e = Proc_Exp x e

call_exp :: Exp -> Exp -> Exp
call_exp e1 e2 = Call_Exp e1 e2