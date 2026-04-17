{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Expr where

type Program = Exp

-- [TODO] Complete data Exp. 
-- data Exp = 
--       ... 
data Exp =
  Const_Exp Int                 -- Expression -> integer_number
  | Diff_Exp Exp Exp            -- Expression -> - integer_number
                                -- Expression -> - (Expression, Expression)
  | Add_Exp Exp Exp             -- Expression -> + (Expression, Expression)
  | Mul_Exp Exp Exp             -- Expression -> * (Expression, Expression)
  | Quo_Exp Exp Exp             -- Expression -> / (Expression, Expression)
  | IsEqual_Exp Exp Exp         -- Expression -> equal? (Expression, Expression)
  | IsGreater_Exp Exp Exp       -- Expression -> greater? (Expression, Expression)
  | IsLess_Exp Exp Exp          -- Expression -> less? (Expression, Expression)
  | IsZero_Exp Exp              -- Expression -> zero? (Expression)
  | If_Exp Exp Exp Exp          -- Expression -> if Expression then Expression else Expression
  | Var_Exp Identifier          -- Expression -> identifier
  | Let_Exp Identifier Exp Exp  -- Expression -> let identifier = Expression in Expression
  deriving Show

type Identifier = String

const_exp :: Int -> Exp
const_exp n = Const_Exp n

diff_exp :: Exp -> Exp -> Exp
diff_exp e1 e2 = Diff_Exp e1 e2

add_exp :: Exp -> Exp -> Exp
add_exp e1 e2 = Add_Exp e1 e2

mul_exp :: Exp -> Exp -> Exp
mul_exp e1 e2 = Mul_Exp e1 e2

quo_exp :: Exp -> Exp -> Exp
quo_exp e1 e2 = Quo_Exp e1 e2

isequal_exp :: Exp -> Exp -> Exp
isequal_exp e1 e2 = IsEqual_Exp e1 e2

isgreater_exp :: Exp -> Exp -> Exp
isgreater_exp e1 e2 = IsGreater_Exp e1 e2

isless_exp :: Exp -> Exp -> Exp
isless_exp e1 e2 = IsLess_Exp e1 e2

iszero_exp :: Exp -> Exp
iszero_exp e = IsZero_Exp e

if_exp :: Exp -> Exp -> Exp -> Exp
if_exp e1 e2 e3 = If_Exp e1 e2 e3

var_exp :: Identifier -> Exp
var_exp s = Var_Exp s

-- let x = exp1 in exp2
let_exp :: Identifier -> Exp -> Exp -> Exp
let_exp x e1 e2 = Let_Exp x e1 e2