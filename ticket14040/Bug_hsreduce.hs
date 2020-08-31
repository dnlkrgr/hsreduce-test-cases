{-# LANGUAGE AllowAmbiguousTypes, GADTs, RankNTypes, TypeApplications, TypeFamilies, TypeInType, TypeOperators #-}
module Eliminator (
    ) where
import Data.Kind
data family A (a :: b)
type a < b = a
data FunArrow = (:->)
class FunType arr where
  type Fun i arr j
class AppType arr where
  type App i arr j (f :: Fun i arr j) (g :: i) :: j
instance FunType (:->) where
  type Fun i (:->) j = i -> j
instance AppType (:->) where
  type App i (:->) j (f) g = f g
k ::
  ()
  -> () -> (forall g (f :: [()]). A () -> A f -> m < () -> ()) -> ()
k = l @(:->) @() @()
l ::
  forall arr a m l.
  ()
  -> ()
     -> (forall f. A () -> A f -> App [()] arr Type m f -> ()) -> ()
l = n
