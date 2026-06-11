module Interp where

import Expr
import Env

type FinalAnswer = ExpVal 

-- Continuation

data Cont =
    End_Cont
  | Let_Exp_Cont Identifier Exp Env Cont Cont
  | If_Test_Cont Exp Exp Env Cont Cont
  | Diff1_Cont Exp Env Cont Cont
  | Diff2_Cont ExpVal Cont Cont
  | Rator_Cont Exp Env Cont Cont
  | Rand_Cont ExpVal Cont Cont
  | Unop_Arg_Cont UnaryOp Cont Cont
  | Try_Cont Identifier Exp Env Cont Cont
  | Raise1_Cont Cont

apply_cont :: Cont -> ExpVal -> FinalAnswer
apply_cont End_Cont v = v
    
apply_cont (Let_Exp_Cont var body env cont exn_cont) val1 =
  value_of_k body (extend_env var val1 env) cont exn_cont

apply_cont (If_Test_Cont exp2 exp3 env cont exn_cont) v =
  let b = expval_bool v in 
    if b == True 
      then value_of_k exp2 env cont exn_cont
      else value_of_k exp3 env cont exn_cont
  
apply_cont (Diff1_Cont exp2 env cont exn_cont) val1 =
  value_of_k exp2 env (Diff2_Cont val1 cont exn_cont) exn_cont

apply_cont (Diff2_Cont val1 cont exn_cont) val2 =
  let num1 = expval_num val1
      num2 = expval_num val2 
  in apply_cont cont (Num_Val (num1 - num2))

apply_cont (Unop_Arg_Cont op cont exn_cont) val =
  apply_cont cont (apply_unop op val)

apply_cont (Rator_Cont rand env cont exn_cont) ratorVal =
  value_of_k rand env (Rand_Cont ratorVal cont exn_cont) exn_cont

apply_cont (Rand_Cont ratorVal cont exn_cont) randVal = 
  let proc = expval_proc ratorVal in
    apply_procedure_k proc randVal cont exn_cont

apply_cont (Try_Cont var handler_exp env cont exn_cont) val = apply_cont cont val

apply_cont (Raise1_Cont exn_cont) val = apply_handler val exn_cont


--
apply_handler :: ExpVal -> Cont -> FinalAnswer
apply_handler val (Try_Cont var handler_exp env saved_cont exn_cont) =
  value_of_k handler_exp (extend_env var val env) saved_cont exn_cont

apply_handler val (End_Cont) = error "Uncaught exception!"


--
apply_unop :: UnaryOp -> ExpVal -> ExpVal 
apply_unop IsZero (Num_Val num) =
  if num == 0 then Bool_Val True else Bool_Val False

apply_unop IsNull (List_Val []) = Bool_Val True
apply_unop IsNull (List_Val _)  = Bool_Val False

apply_unop Car (List_Val (x:_)) = x

apply_unop Cdr (List_Val (_:xs)) = List_Val xs


--
value_of_k :: Exp -> Env -> Cont -> Cont -> FinalAnswer

value_of_k (Const_Exp n) env cont exn_cont = apply_cont cont (Num_Val n)

value_of_k (Const_List_Exp nums) env cont exn_cont = apply_cont cont (List_Val [ Num_Val num | num <- nums ])

value_of_k (Var_Exp var) env cont exn_cont =
  let val = apply_env env var in
    apply_cont cont val

value_of_k (Diff_Exp exp1 exp2) env cont exn_cont =
  value_of_k exp1 env (Diff1_Cont exp2 env cont exn_cont) exn_cont

value_of_k (Unary_Exp op exp1) env cont exn_cont =
  value_of_k exp1 env (Unop_Arg_Cont op cont exn_cont) exn_cont
  
value_of_k (If_Exp exp1 exp2 exp3) env cont exn_cont =
  value_of_k exp1 env (If_Test_Cont exp2 exp3 env cont exn_cont) exn_cont

value_of_k (Let_Exp var exp1 body) env cont exn_cont =
  value_of_k exp1 env (Let_Exp_Cont var body env cont exn_cont) exn_cont

value_of_k (Letrec_Exp proc_name bound_var proc_body letrec_body) env cont exn_cont =
  value_of_k letrec_body (extend_env_rec proc_name bound_var proc_body env) cont exn_cont

value_of_k (Proc_Exp var body) env cont exn_cont =
  apply_cont cont (Proc_Val (Procedure var body env))

value_of_k (Call_Exp rator rand) env cont exn_cont =
  value_of_k rator env (Rator_Cont rand env cont exn_cont) exn_cont
  
value_of_k (Try_Exp exp var handler_exp) env cont exn_cont = 
  let try_cont = Try_Cont var handler_exp env cont exn_cont
  in value_of_k exp env try_cont try_cont

value_of_k (Raise_Exp exp) env cont exn_cont =
  value_of_k exp env (Raise1_Cont exn_cont) exn_cont


--
value_of_program :: Exp -> ExpVal

value_of_program exp =
  value_of_k exp initEnv End_Cont End_Cont


--
initEnv = extend_env "i" (Num_Val 1)
            (extend_env "v" (Num_Val 5)
              (extend_env "x" (Num_Val 10) empty_env))


--
apply_procedure_k :: Proc -> ExpVal -> Cont -> Cont -> FinalAnswer
apply_procedure_k (Procedure var body saved_env) argVal cont exn_cont =
  value_of_k body (extend_env var argVal saved_env) cont exn_cont
