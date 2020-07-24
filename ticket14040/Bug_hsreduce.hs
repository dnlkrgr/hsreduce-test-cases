{-# LANGUAGE AllowAmbiguousTypes, RankNTypes, TypeApplications, TypeFamilies, TypeInType, TypeOperators #-}
module Eliminator (
    ) where
import Data.Kind
data family Sing a
type family Apply f x
type a @@ b = Apply () ()
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
  () -> () -> (forall xs. Sing () -> Sing () -> () -> () @@ ()) -> ()
listElimTyFun = listElimPoly @(:->) @()
listElimPoly ::
  ()
  -> ()
     -> (forall x. Sing () -> Sing () -> () -> App () arr Type p (x))
        -> ()
listElimPoly _ = undefined
