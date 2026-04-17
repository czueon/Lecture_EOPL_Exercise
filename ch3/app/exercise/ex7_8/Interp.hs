{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interp where

import Expr
import Env

{- data ExpVal = Num_Val { expval_num :: Int } | Bool_Val { expval_bool :: Bool } -}

--
value_of :: Exp -> Env -> ExpVal

value_of (Const_Exp n) env = Num_Val n

value_of (Diff_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Num_Val (n1 - n2)

value_of (Add_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Num_Val (n1 + n2)

value_of (Mul_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Num_Val (n1 * n2)

value_of (Quo_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Num_Val (n1 `div` n2)

value_of (IsEqual_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Bool_Val (n1 == n2)

value_of (IsGreater_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Bool_Val (n1 > n2)

value_of (IsLess_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Bool_Val (n1 < n2)

value_of (IsZero_Exp exp) env =   -- Ex) zero?(0), zero?(1)
  let val1 = value_of exp env     -- val1 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
  in if n1 == 0
      then Bool_Val True          -- True :: Bool
      else Bool_Val False         -- False :: Bool

-- exp1 : unconditional exp / exp2, exp3 : conditional exp
value_of (If_Exp exp1 exp2 exp3) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      b = expval_bool val1        -- b :: Bool
  in if b
     then value_of exp2 env       -- val2 :: ExpVal
     else value_of exp3 env       -- val3 :: ExpVal else val3

value_of (Var_Exp var) env = 
  apply_env env var               -- apply_env :: Env -> Identifier -> ExpVal

value_of (Let_Exp var exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
  in value_of exp2 (extend_env var val1 env)

--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  value_of exp
    (extend_env "x" (Num_Val 10)
      (extend_env "v" (Num_Val 5)
        (extend_env "i" (Num_Val 1)
          empty_env)))

-- [TODO] Complete initEnv.
--   { x |-> 10, v |-> 5, i |-> 1 }
initEnv :: Env
initEnv = undefined
