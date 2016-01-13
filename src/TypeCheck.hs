module TypeCheck where

import AST
import Control.Monad (when)
import qualified Data.Map as M

type EitherS = Either String

typeCheck :: Context -> Term -> EitherS (Type, Context)
typeCheck ctx (TmVar x) =
    case M.lookup x ctx of
        Just (Linear prety) -> Right (Linear prety, M.delete x ctx) -- (A-LVAR)
        Just (Unres  prety) -> Right (Unres prety,  ctx)            -- (A-UVAR)
        Nothing -> Left $ "No such variable: " ++ show x

typeCheck ctx (TmBool q b) = Right (Type q TyBool, ctx)             -- (A-BOOL)

typeCheck ctx (TmCond t1 t2 t3) = do                                -- (A-IF)
    (Type q prety, ctx') <- typeCheck ctx t1
    if prety == TyBool
        then do
            (ty2, ctx'')  <- typeCheck ctx' t2
            (ty3, ctx''') <- typeCheck ctx'' t3
            if ty2 == ty3
                then return (ty2, ctx''')
                else fail $ "Condition branch types don't unify: " ++ show ty2 ++ ", " ++ show ty3
        else fail $ "Wrong type of condition for if: " ++ show prety

typeCheck ctx (TmPair t1 t2) = do                                   -- (A-PAIR)
    (ty1@(Type q1 _), ctx')  <- typeCheck ctx  t1
    (ty2@(Type q2 _), ctx'') <- typeCheck ctx' t2
    let q = q1 /\ q2
    return (Type q (TyPair ty1 ty2), ctx'')

typeCheck ctx (TmSplit t1 x y t2) = do                              -- (A-SPLIT)
    (Type q prety1, ctx') <- typeCheck ctx t1
    case prety1 of
        TyPair tyx tyy -> do
            let ctx'' = M.insert x tyx $ M.insert y tyy ctx'
            (ty2, ctx''') <- typeCheck ctx'' t2
            return (ty2, ctx''' `ctxdiff` M.fromList [(x, tyx), (y, tyy)])
        _ -> fail $ "Wrong type for splitting: " ++ show prety1

typeCheck ctx (TmAbs q x ty1 t2) = do                               -- (A-ABS)
    (ctx', ty2) <- typeCheck (M.insert x ty1 ctx) t2
    when (q == Unres) $ do
        let (Just ty) = M.lookup x ctx'
        case ty of
            Type Unres  _ -> return ()
            Type Linear _ -> fail $ "Unrestricted abstraction can't contain restricted var: " ++ show x
    return $ (Type q (TyArrow ty1 ty2), M.delete x ctx')

typeCheck ctx (TmApp t1 t2) = do
    (Type q ty1, ctx') <- typeCheck ctx t1
    case ty1 of
        TyArrow ty11 ty12 -> do
            (ty11', ctx'') <- typeCheck ctx' t2
            if ty11 == ty11'
                then return (ty12, ctx'')
                else fail $ "Unmatched type for application: " ++
                            show ty11 ++ " and " show ty11'
        _ -> fail $ "Wrong type for application: " ++ show ty1

-- lin ⊑ un
(/\) :: Qualifier -> Qualifier -> Qualifier
(/\) Unres Unres = Unres
(/\) Linear _    = Linear

-- (÷)
ctxdiff :: Context -> Context -> Context
ctxdiff ctx ctx' = ctxdiff' ctx $ M.toList ctx'

ctxdiff' :: Context -> [(Name, Type)] -> Context
ctxdiff' ctx [] = ctx
ctxdiff' ctx ((x, ty@(Type Unres _) : ps)) =
    let ctx' = ctx `ctxdiff'` ps
    in  M.delete x ctx'
ctxdiff' ctx ((x, ty@(Type Linear _) : ps)) =
    let ctx' = ctx `ctxdiff'` ps
    in  case M.lookup x ctx' of
            Just (Type Linear _) ->
                error $ "ctxdiff undefined: " ++
                        show x ++ " : " ++ show ty
            Nothing -> ctx'

