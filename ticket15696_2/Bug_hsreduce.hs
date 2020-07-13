{-# LANGUAGE BangPatterns #-}
module Main (
        main
    ) where
import qualified Data.Foldable
import qualified GHC.Classes
import qualified GHC.Enum
import qualified GHC.Magic
import qualified GHC.Read
import qualified GHC.Show
import qualified GHC.Types
import qualified System.IO
data Set_DataSetInternal a
  = Bin_DataSetInternal {-# UNPACK #-} !Size_DataSetInternal !a !(Set_DataSetInternal a) !(Set_DataSetInternal a) |
    Tip_DataSetInternal
type Size_DataSetInternal = GHC.Types.Int
singleton_DataSetInternal x
  = Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
insert_DataSetInternal x0
  = go x0 undefined
  where
      go orig _ Tip_DataSetInternal
        = singleton_DataSetInternal (GHC.Magic.lazy orig)
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
        = Data.Foldable.foldl' ins t0 xs
        where
            ins t x = insert_DataSetInternal x t
instance GHC.Show.Show a =>
         GHC.Show.Show (Set_DataSetInternal a) where
  showsPrec p xs
    = GHC.Show.showParen (p > 10) $ GHC.Show.showString "fromList "
        . GHC.Show.shows (toList_DataSetInternal xs)
balanceR_DataSetInternal x _ r
  = Bin_DataSetInternal 2 x Tip_DataSetInternal r
main
  = System.IO.print
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
  deriving (GHC.Show.Show,
            GHC.Read.Read,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            GHC.Enum.Bounded,
            GHC.Enum.Enum)
