{-# LANGUAGE TypeApplications, TypeFamilies, TypeInType #-}
module Eliminator (
    ) where
data FunArrow = (:->)
class FunType arr where
  type Fun k1 arr k2
class AppType arr where
  type App k1 arr k2 (f :: Fun k1 arr ()) (x :: k1) :: ()
instance FunType (:->) where
  type Fun k1 (:->) () = k1 -> ()
instance AppType (:->) where
  type App _ (:->) () (f) x = f x
listElimTyFun :: () -> () -> (() -> () -> _ -> ()) -> ()
listElimTyFun = listElimPoly @(:->)
listElimPoly ::
  () -> () -> (() -> () -> App _ arr () () () -> ()) -> ()
listElimPoly = undefined
