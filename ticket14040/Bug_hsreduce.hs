{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, TypeApplications, TypeFamilies, TypeInType, TypeOperators #-}
module Eliminator (
    ) where
import Data.Kind
data family Sing a
data FunArrow = (:->)
class FunType arr where
  type Fun k1 arr k2
class AppType arr where
  type App k1 arr k2 (f :: Fun k1 arr k2) (x :: k1) :: k2
instance FunType (:->) where
  type Fun k1 (:->) k2 = k1 -> k2
instance AppType (:->) where
  type App k1 (:->) k2 (f) x = f x
type (-?>) k1 k2 arr = Fun k1 arr k2
listElimTyFun ::
  () -> () -> (forall xs. Sing () -> Sing () -> p -> ()) -> ()
listElimTyFun = listElimPoly @(:->) @() @()
listElimPoly ::
  forall a (p :: ([a] -?> Type) arr).
  ()
  -> ()
     -> (forall xs. Sing () -> Sing () -> App [a] arr Type p xs -> ())
        -> ()
listElimPoly = undefined
