{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Env where

import Expr

data Env =
    Empty_Env
  | Extend_Env Identifier ExpVal Env
  deriving (Show)


empty_env :: Env
empty_env = Empty_Env

extend_env :: Identifier -> ExpVal -> Env -> Env
extend_env x v env = Extend_Env x v env

-- case expression
-- apply_env :: Env -> Identifier -> ExpVal
-- apply_env env x =
--   case env of
--     Empty_Env -> error ("Not found: " ++ x)
--     Extend_Env var v env1 ->
--       if x == var then v
--       else apply_env env1 x

-- pattern matching
apply_env :: Env -> Identifier -> ExpVal
apply_env (Empty_Env) x = error ("Not found: " ++ x)
apply_env (Extend_Env var v env1) x =
  if x == var then v
  else apply_env env1 x

data ExpVal =
    Num_Val  { expval_num  :: Int   } 
  | Bool_Val { expval_bool :: Bool }
   
instance Show ExpVal where
  show (Num_Val num)   = show num
  show (Bool_Val bool) = show bool

type DenVal = ExpVal   
