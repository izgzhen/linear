module Example where

import TypeCheck
import AST

-- double free:
-- λx. λfree.((λz.λy.<free z,free y>) x x)
tm1 = TmAbs Linear "x" (Type Linear TyBool) $
        TmAbs Unres "free" (Type Unres (TyArrow (Type Linear TyBool) (Type Unres TyUnit))) $
            TmApp
                (TmApp
                    (
                        TmAbs Linear "z" (Type Linear TyBool) $
                            TmAbs Linear "y" (Type Linear TyBool) $
                                TmPair
                                    Linear
                                    (TmApp (TmVar "free") (TmVar "z"))
                                    (TmApp (TmVar "free") (TmVar "y"))
                    )
                    (TmVar "x")
                )
                (TmVar "x")

{-
    Explanation for this example:

    It will fail due to the missing of "x" from type context. This is reasonable, since
    it is "used" once already, thus can't be used in another sub-context again.
-}

------------------------------------------------------------------------------------------

{- linear containment in unrestricted:

 let z = un <x,3> in
  split z as x1,_ in
  split z as x2,_ in
  <free x1,free x2>

-}


tm2 = TmAbs Linear "x" (Type Linear TyBool) $
        TmAbs Unres "free" (Type Unres (TyArrow (Type Linear TyBool) (Type Unres TyUnit))) $
            TmApp
                (
                    TmAbs Unres "z" (Type Unres (TyPair (Type Linear TyBool) (Type Unres TyBool))) $
                        TmSplit (TmVar "z") "x1" "_y1" $
                            TmSplit (TmVar "z") "x2" "_y2" $
                                TmPair
                                    Linear
                                    (TmApp (TmVar "free") (TmVar "x1"))
                                    (TmApp (TmVar "free") (TmVar "x2"))
                )
                (TmPair Unres (TmVar "x") (TmBool Unres TRUE))



{-
    Explanation for this example:

    Result is "Containment is violated in TmPair". It is because "x" is linear but contained inside 
    a unrestricted pair. So it can be fetch twice.
-}

