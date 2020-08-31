module Main (
        main
    ) where
import Data.Foldable as Foldable
main
  = print $ foldl' (flip m) (q a) b
  where
      f _ = T2
      a = f undefined
      b = [f undefined]
data T
  = A | T2
  deriving Show
data K a
  = Bin !a !(K ()) !(K a) | Tip
  deriving Show
m n
  = o n undefined
  where
      o p _ Tip = q p
      o p _ t@(Bin y l r) = Bin y l (o p undefined r)
q x = Bin x Tip Tip