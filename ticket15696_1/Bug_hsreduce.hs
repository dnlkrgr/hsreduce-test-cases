module Main (
        main
    ) where
import Data.Foldable as Foldable
main
  = print $ foldl' (flip wumbo) (singleton a) b
  where
      f _ = T2
      a = f ()
      b = [f ()]
data T
  = T2
  deriving Show
data Set a
  = Bin !a !(Set a) !(Set a) | Tip
  deriving Show
wumbo x0
  = go x0 x0
  where
      go orig _ Tip = singleton orig
      go orig x t@(Bin y l r) = Bin y l (go orig x r)
singleton x = Bin x Tip Tip