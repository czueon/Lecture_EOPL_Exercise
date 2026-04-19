{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interp where

import Expr
import Env
import Store

--
value_of :: Exp -> Env -> Store -> (ExpVal, Store)   -- Sec 4.2.1: Store-passing specifications

value_of (Const_Exp n) env store = (Num_Val n, store)

value_of (Var_Exp var) env store = (apply_env env var, store)

value_of (Diff_Exp exp1 exp2) env store =
  let (val1, store1) = value_of exp1 env store
      (val2, store2) = value_of exp2 env store1
      n1 = expval_num val1
      n2 = expval_num val2
  in (Num_Val (n1 - n2), store2)
  
value_of (IsZero_Exp exp) env store =
  let (val1, store1) = value_of exp env store
      n1 = expval_num val1
  in if n1 == 0 
     then (Bool_Val True, store1) 
     else (Bool_Val False, store1)

value_of (If_Exp exp1 exp2 exp3) env store =
  let (val1, store1) = value_of exp1 env store
      b = expval_bool val1
  in if b
     then value_of exp2 env store1 
     else value_of exp3 env store1

value_of (Let_Exp var exp1 body) env store =
  let (val1, store1) = value_of exp1 env store
  in value_of body (extend_env var val1 env) store1

value_of (Letrec_Exp letbindings letrec_body) env store =
  value_of letrec_body (extend_env_rec letbindings env) store

value_of (Proc_Exp var body) env store =
  (Proc_Val (Procedure var body env), store)

value_of (Call_Exp rator rand) env store =
  let (val1, store1) = value_of rator env store
      (val2, store2) = value_of rand env store1

      proc = expval_proc val1
      arg = val2
  in apply_procedure proc arg store2

value_of (Block_Exp [exp]) env store = 
  value_of exp env store

value_of (Block_Exp (exp:expList)) env store =
  let (val1, store1) = value_of exp env store
  in value_of (Block_Exp expList) env store1

value_of (Newref_Exp exp) env store =
  let (val1, store1) = value_of exp env store
      (loc2, store2) = newref store1 val1
  in (Ref_Val loc2, store2)

value_of (Deref_Exp exp) env store =
  let (val1, store1) = value_of exp env store
      loc1 = expval_loc val1
      val2 = deref store1 loc1
  in (val2, store1)

value_of (Setref_Exp exp1 exp2) env store =
  let (val1, store1) = value_of exp1 env store
      (val2, store2) = value_of exp2 env store1
      loc1 = expval_loc val1
      store3 = setref store2 loc1 val2
  in (val2, store3)

--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  let (val, store) = value_of exp initEnv initStore
  in val


--
initEnv = extend_env "i" (Num_Val 1)
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10) empty_env))

--
apply_procedure :: Proc -> ExpVal -> Store -> (ExpVal,Store)
apply_procedure proc arg store =
  value_of (body proc) (extend_env (var proc) arg (saved_env proc)) store
