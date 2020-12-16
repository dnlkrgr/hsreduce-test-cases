
module Main (
        main
    ) where
import Data.Foldable as Foldable
main
  = print $ Foldable.foldl' (flip wumbo) (singleton a) b
  where
      f = T2
      a = f
      b = [f]
data T
  = T2
  deriving Show
data Set a
  = Bin !a !(Set ()) !(Set a) | Tip
  deriving Show
wumbo x0
  = go x0 undefined
  where
      go orig _ Tip = singleton orig
      go orig _ t@(Bin y l r) = Bin y l (go orig undefined r)
singleton x = Bin x Tip Tip
