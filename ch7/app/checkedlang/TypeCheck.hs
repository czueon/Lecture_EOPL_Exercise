module TypeCheck where

import qualified Data.Map as Map
import Expr

--
typeCheck :: Exp -> IO (Either String Type)
typeCheck exp = return (type_of_program exp )
     
-- data Either String Type = 
--         Left String
--       | Right Type
--
type_of_program :: Exp -> Either String Type
type_of_program exp = type_of exp empty_tyenv

init_tyenv =
  extend_tyenv "i" TyInt
    (extend_tyenv "v" TyInt
      (extend_tyenv "x" TyInt
        empty_tyenv))

--
type TyEnv = Map.Map Identifier Type

type_of :: Exp -> TyEnv -> Either String Type

type_of (Const_Exp n) tyenv = Right TyInt    -- TyInt :: Type

type_of (Var_Exp var) tyenv = apply_tyenv tyenv var

type_of (Diff_Exp exp1 exp2) tyenv =
  do ty1 <- type_of exp1 tyenv
     ty2 <- type_of exp2 tyenv
     if equalType ty1 TyInt && equalType ty2 TyInt
     then Right TyInt        -- TyInt :: Type    Right TyInt :: Either String Type
     else Left "Type mismatch in exp1 or exp2"
                             -- Left String :: Either String Type

type_of (IsZero_Exp exp1) tyenv =
  do ty1 <- type_of exp1 tyenv
     if equalType ty1 TyInt
     then Right TyBool       -- TyBool :: Type   Right TyBool :: Either String Type
     else Left "Type mismatch expected int but ..."

type_of (If_Exp exp1 exp2 exp3) tyenv =
  do t1 <- type_of exp1 tyenv
     t2 <- type_of exp2 tyenv
     t3 <- type_of exp3 tyenv
     if equalType t1 TyBool && equalType t2 t3
     then Right t2
     else Left "Type mismatch ..."

type_of (Let_Exp var exp1 body) tyenv =
  do ty1 <- type_of exp1 tyenv
     type_of body (extend_tyenv var ty1 tyenv)

type_of (Letrec_Exp res_ty f x arg_ty body exp) tyenv  =
  do ty0 <- type_of body (extend_tyenv f (TyFun arg_ty res_ty)
                    (extend_tyenv x arg_ty
                      tyenv))
     -- ty0 == res_ty
     ty1 <- type_of exp (extend_tyenv f (TyFun arg_ty res_ty) tyenv)
     if equalType ty0 res_ty
     then Right ty1
     else Left ("Type mismatch: Expected " ++ show res_ty ++ " but met " ++ show ty0)

type_of (Proc_Exp var argTy body) tyenv =
  do resTy <- type_of body (extend_tyenv var argTy tyenv)
     Right (TyFun argTy resTy)

type_of (Call_Exp rator rand) tyenv =
  do ty1 <- type_of rator tyenv
     ty2 <- type_of rand tyenv
     case ty1 of
       TyFun argTy resTy ->
         if equalType argTy ty2
         then Right resTy
         else Left ("Argument type mismatch: Expected " ++ show argTy ++ " but met " ++ show ty2)
       TyInt -> Left "Type mismatch: Expected a function type but met int"
       TyBool -> Left "Type mismatch: Expected a function type but met bool"

         
-- Utilities
apply_tyenv :: TyEnv -> Identifier -> Either String Type 
apply_tyenv tyenv var =
  case Map.lookup var tyenv of
    Just ty -> Right ty
    Nothing -> Left $ "Variable not found: " ++ var

empty_tyenv :: TyEnv 
empty_tyenv = Map.empty 

extend_tyenv :: Identifier -> Type -> TyEnv -> TyEnv
extend_tyenv var ty tyenv = Map.insert var ty tyenv

expectedButErr :: Type -> Type -> Exp -> Either String Type
expectedButErr expectedTy gotTy exp =
  Left $ "Expected " ++ show expectedTy ++ " but got " ++ show gotTy ++ " in " ++ show exp

expectedFuntyButErr :: Type -> Exp -> Either String Type
expectedFuntyButErr gotTy exp =
  Left $ "Expected function type but got " ++ show gotTy ++ " in " ++ show exp

inequalIfBranchTyErr :: Type -> Type -> Exp -> Exp -> Either String Type
inequalIfBranchTyErr thenTy elseTy exp2 exp3 =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show thenTy ++ " in " ++ show exp2
          ++ "\t" ++ show elseTy ++ " in " ++ show exp3

inequalArgtyErr :: Type -> Type -> Exp -> Exp -> Either String Type
inequalArgtyErr argTy1 argTy2 funexp argexp =
  Left $ "Type mismatch: \n"
          ++ "\t" ++ show argTy1 ++ " for the arugment of " ++ show funexp
          ++ "\t" ++ show argTy2 ++ " in " ++ show argexp

equalType :: Type -> Type -> Bool
equalType TyInt  TyInt  = True
equalType TyBool TyBool = True
equalType (TyFun ty1 ty1') (TyFun ty2 ty2') =
  equalType ty1 ty2 && equalType ty1' ty2'
equalType _ _ = False

