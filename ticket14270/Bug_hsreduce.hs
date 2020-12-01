{-# LANGUAGE TypeInType, ViewPatterns, TypeOperators, RankNTypes, PatternSynonyms, FlexibleInstances, GADTs, ScopedTypeVariables, UndecidableInstances, TypeApplications #-}
module DataTypeableInternal (
        pattern App
    ) where
data TypeRep (a :: k)
  where TrFun :: () -> () -> () -> TypeRep (() -> ())
pattern App f x <- (splitApp -> Just (IsApp f x)) where
                  App _ = undefined
data IsApp a where IsApp :: TypeRep f -> () -> IsApp (f ())
splitApp :: TypeRep a -> Maybe (IsApp a)
splitApp rep@(TrFun _ _ _) = Just (IsApp undefined undefined)
