{-# LANGUAGE BangPatterns #-}
module Main (
        main
    ) where
import Data.Foldable as Foldable
data Set_DataSetInternal a
  = Bin_DataSetInternal a (Set_DataSetInternal a) (Set_DataSetInternal a) |
    Tip_DataSetInternal
insert_DataSetInternal x0
  = go x0 undefined
  where
      go orig _ Tip_DataSetInternal
        = Bin_DataSetInternal orig Tip_DataSetInternal Tip_DataSetInternal
      go orig _ t@(Bin_DataSetInternal y _ r)
        = case undefined of {
            _ -> Bin_DataSetInternal y Tip_DataSetInternal r'
              where
                  !r' = go orig undefined r }
foldr_DataSetInternal f
  = go
  where
      go z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal x l r) = go (f x (go z' r)) l
fromList_DataSetInternal (x0 : xs0)
  = fromList'
      (Bin_DataSetInternal x0 undefined Tip_DataSetInternal) xs0
  where
      fromList'
        = Foldable.foldl' ins
        where
            ins t x = insert_DataSetInternal x t
instance Show a => Show (Set_DataSetInternal a) where
  showsPrec _ xs
    = showString "fromList " . shows (foldr_DataSetInternal (:) [] xs)
main
  = print
      $ let f _ = T2_Main
        in fromList_DataSetInternal [f undefined, f undefined]
data T_Main
  = T2_Main
  deriving Show
