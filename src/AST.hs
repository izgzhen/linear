module AST where

import qualified Data.Map as M

data Qualifier = Linear | Unres deriving (Eq)

data Boolean = TRUE | FALSE deriving (Show, Eq)

type Name = String

data Term = TmVar Name
          | TmBool Qualifier Boolean
          | TmCond Term Term Term
          | TmPair Qualifier Term Term
          | TmSplit Term Name Name Term
          | TmAbs Qualifier Name Type Term
          | TmApp Term Term
          deriving (Show, Eq)

data Pretype = TyBool
             | TyUnit
             | TyPair Type Type
             | TyArrow Type Type
             deriving (Eq)

data Type = Type Qualifier Pretype deriving (Eq)

type Context = M.Map Name Type

instance Show Qualifier where
    show Linear = "lin"
    show Unres  = "un"

instance Show Pretype where
    show TyBool = "Bool"
    show TyUnit = "()"
    show (TyPair t1 t2)  = "(" ++ show t1 ++ ", "   ++ show t2 ++ ")"
    show (TyArrow t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

instance Show Type where
    show (Type q pretype) = show q ++ " " ++ show pretype
