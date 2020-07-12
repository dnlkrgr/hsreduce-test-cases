{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, TypeApplications, TypeFamilies, TypeInType #-}
module Eliminator (
    ) where
import Data.Kind
data family Sing a
data FunArrow = (:->)
class FunType arr where
  type Fun k1 arr k2
class () => AppType arr where
  type App k1 arr k2 (f :: Fun () arr k2) (x :: ()) :: k2
instance FunType (:->) where
  type Fun () (:->) k2 = () -> k2
instance AppType (:->) where
  type App () (:->) k2 f x = f x
listElimTyFun ::
  () -> () -> (forall xs. Sing () -> Sing () -> () -> p) -> ()
listElimTyFun = listElimPoly @(:->) @() @() @()
listElimPoly ::
  forall arr a p l.
  ()
  -> ()
     -> (forall xs. Sing () -> Sing () -> () -> App () arr Type p (xs))
        -> ()
listElimPoly _ _ _ = undefined
