{-# LANGUAGE TypeInType, ViewPatterns, PatternSynonyms, GADTs #-}
module DataTypeableInternal (
    ) where
data A (a :: b)
  where
    C :: () -> () -> () -> A ()
    D :: () -> () -> () -> A ()
    E :: () -> () -> () -> A (() -> ())
pattern App f h <- (f -> Just (G f h)) where
                  App _ _ = undefined
data G a where G :: A f -> () -> G (f ())
f :: A a -> Maybe (G a)
f i@(E _ _ _) = Just (G undefined undefined)
