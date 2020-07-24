{-# LANGUAGE BangPatterns #-}
module Main (
        main
    ) where
import Data.Foldable as Foldable
data Set_DataSetInternal a
  = Bin_DataSetInternal {-# UNPACK #-} !Size_DataSetInternal !a !(Set_DataSetInternal a) !(Set_DataSetInternal a) |
    Tip_DataSetInternal
type Size_DataSetInternal = Int
singleton_DataSetInternal x
  = Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
insert_DataSetInternal x0
  = go x0 undefined
  where
      go orig _ Tip_DataSetInternal = singleton_DataSetInternal (orig)
      go orig _ t@(Bin_DataSetInternal _ y _ r)
        = case undefined of {
            _ -> balanceR_DataSetInternal y undefined r'
              where
                  !r' = go orig undefined r }
foldr_DataSetInternal f z
  = go z
  where
      go z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal _ x l r) = go (f x (go z' r)) l
toList_DataSetInternal = toAscList_DataSetInternal
toAscList_DataSetInternal = foldr_DataSetInternal (:) []
fromList_DataSetInternal (x0 : xs0)
  = fromList'
      (Bin_DataSetInternal 1 x0 Tip_DataSetInternal Tip_DataSetInternal)
      xs0
  where
      fromList' t0 xs
        = foldl' ins t0 xs
        where
            ins t x = insert_DataSetInternal x t
instance Show a => Show (Set_DataSetInternal a) where
  showsPrec _ xs
    = showString "fromList " . shows (toList_DataSetInternal xs)
balanceR_DataSetInternal x _ r
  = Bin_DataSetInternal 2 x Tip_DataSetInternal r
main
  = print
      $ let f _ = T2_Main
        in fromList_DataSetInternal [f undefined, f undefined]
data T_Main
  = T1_Main |
    T2_Main |
    T3_Main |
    T4_Main |
    T5_Main |
    T6_Main |
    T7_Main |
    T8_Main |
    T9_Main
  deriving Show
