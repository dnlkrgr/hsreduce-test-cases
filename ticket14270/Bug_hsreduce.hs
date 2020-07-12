{-# LANGUAGE TypeInType, ViewPatterns, PatternSynonyms, GADTs, ScopedTypeVariables #-}
module DataTypeableInternal (
    ) where
data TypeRep (a :: k)
  where
    TrFun :: forall r1 r2 a b. () -> () -> () -> TypeRep (() -> ())
pattern App f x <- (splitApp -> Just (IsApp f x)) where
                  App _ _ = undefined
data IsApp a
  where IsApp :: forall k k' f x. TypeRep f -> () -> IsApp (f ())
splitApp :: TypeRep a -> Maybe (IsApp a)
splitApp rep@(TrFun _ _ _) = Just (IsApp undefined undefined)
