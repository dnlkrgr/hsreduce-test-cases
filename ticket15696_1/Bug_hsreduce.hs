module Main (
        main
    ) where
import Data.Foldable as Foldable
main
  = print $ foldl' (flip wumbo) (singleton a) b
  where
      f _ = T2
      a = f undefined
      b = [f undefined]
data T
  = T1 | T2
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