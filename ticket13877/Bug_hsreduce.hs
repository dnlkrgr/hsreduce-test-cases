{-# LANGUAGE AllowAmbiguousTypes, ConstraintKinds, GADTs, RankNTypes, ScopedTypeVariables, TemplateHaskell, Trustworthy, TypeApplications, TypeFamilies, TypeInType, TypeOperators #-}
module Eliminator where
data FunArrow = (:->)
class FunType arr where
  type Fun k1 arr k2
class AppType arr where
  type App k1 arr k2 (f :: Fun k1 arr k2) (x :: k1) :: k2
instance FunType (:->) where
  type Fun k1 (:->) k2 = k1 -> k2
instance AppType (:->) where
  type App _ (:->) _ (f) x = f x
listElimTyFun :: _ -> _ -> (_ -> _ -> _ -> _) -> _
listElimTyFun = listElimPoly @(:->)
listElimPoly :: _ -> _ -> (_ -> _ -> _ -> App _ arr _ () _) -> _
listElimPoly = undefined
