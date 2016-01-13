module AST where

import qualified Data.Map as M

data Qualifier = Linear | Unres deriving (Show, Eq)

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
             | TyPair Type Type
             | TyArrow Type Type
             deriving (Show, Eq)

data Type = Type Qualifier Pretype deriving (Show, Eq)

type Context = M.Map Name Type


