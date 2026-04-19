{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Eta reduce" #-}
module Env where

import Expr (Identifier,Exp)

data Env =
    Empty_Env
  | Extend_Env Identifier ExpVal Env
  | Extend_Env_Rec Identifier Identifier Exp Env
  deriving (Show)

empty_env :: Env
empty_env = Empty_Env

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = Extend_Env x v env

apply_env :: Env -> Identifier -> ExpVal
apply_env (Empty_Env) x = error ("Not found: " ++ x)
apply_env (Extend_Env saved_var saved_val saved_env) search_var
  | search_var == saved_var = saved_val
  | otherwise = apply_env saved_env search_var
apply_env (Extend_Env_Rec p_name b_var p_body saved_env) search_var
  | search_var == p_name = Proc_Val (Procedure b_var p_body (Extend_Env_Rec p_name b_var p_body saved_env))
  | otherwise = apply_env saved_env search_var


extend_env_rec :: Identifier -> Identifier -> Exp -> Env -> Env
extend_env_rec f x exp env = Extend_Env_Rec f x exp env

data ExpVal =
    Num_Val  { expval_num  :: Int  } 
  | Bool_Val { expval_bool :: Bool }
  | Proc_Val { expval_proc :: Proc }
   
instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool
  show (Proc_Val proc) = "<proc>"

-- Denoted values
type DenVal = ExpVal   

-- Procedure values : data structures
data Proc = Procedure {var :: Identifier, body :: Exp, saved_env :: Env}

-- procedure :: Identifier -> Exp -> Env -> Proc
-- procedure var body env = Procedure var body env

-- In Interp.hs
-- apply_procedure :: Proc -> ExpVal -> ExpVal
  
