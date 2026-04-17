{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Interp where

import Expr
import Env

--
value_of :: Exp -> Env -> ExpVal

value_of (Const_Exp n) env = Num_Val n

value_of (Diff_Exp exp1 exp2) env =
  let val1 = value_of exp1 env    -- val1 :: ExpVal
      val2 = value_of exp2 env    -- val2 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
      n2 = expval_num val2        -- n2 :: Int
  in Num_Val (n1 - n2)

value_of (IsZero_Exp exp) env =   -- Ex) zero?(0), zero?(1)
  let val1 = value_of exp env     -- val1 :: ExpVal
      n1 = expval_num val1        -- n1 :: Int
  in if n1 == 0
      then Bool_Val True          -- True :: Bool
      else Bool_Val False         -- False :: Bool

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

value_of (Proc_Exp var body) env =
  Proc_Val (Procedure var body env)

value_of (Call_Exp rator rand) env =
  let proc = expval_proc (value_of rator env)  -- proc :: Proc
      arg = value_of rand env                  -- arg :: ExpVal
  in apply_procedure proc arg

--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  value_of exp
    (extend_env "x" (Num_Val 10)
      (extend_env "v" (Num_Val 5)
        (extend_env "i" (Num_Val 1)
          empty_env)))

initEnv :: Env
initEnv = undefined

--
apply_procedure :: Proc -> ExpVal -> ExpVal
apply_procedure (Procedure var body env0) arg =
  value_of body (extend_env var arg env0)