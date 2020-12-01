import Data.Foldable  
data Set_DataSetInternal a
  = Bin_DataSetInternal  Size_DataSetInternal a (Set_DataSetInternal a) (Set_DataSetInternal a) |
    Tip_DataSetInternal
type Size_DataSetInternal = Int
singleton_DataSetInternal b
  = Bin_DataSetInternal 1 b Tip_DataSetInternal Tip_DataSetInternal
insert_DataSetInternal x0
  = go x0 x0
  where
      go c k Tip_DataSetInternal
        = singleton_DataSetInternal c
      go c b (Bin_DataSetInternal d g l r)
        = e g l r'
              where
                  r' = go c b r
i f 
  = go 
  where
      go z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal _ x l r) = go (f x (go z' r)) l
toList_DataSetInternal = i (:) []
h (x0 : xs0)
  = fromList'
      (Bin_DataSetInternal 1 x0 Tip_DataSetInternal Tip_DataSetInternal)
      xs0
  where
      not_ordered x (y : _) = x >= y
      fromList' t0 xs
        = foldl' ins t0 xs
        where
            ins t x = insert_DataSetInternal x t
      create s xs@(x : xss)
        = if not_ordered x xss then
              (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal, 
               [], xss)
          else
              (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal, 
               xss, [])
instance Show a => Show (Set_DataSetInternal a) where
  showsPrec p xs
    = showParen (p > 0) $ showString "fromList "
        . shows (toList_DataSetInternal xs)
splitMember_DataSetInternal x (Bin_DataSetInternal _ y l r)
  = case compare x y of
      LT
        -> let
             (lt, found, gt) = splitMember_DataSetInternal x l
             lt' = l in (lt', found, gt)
insertMin_DataSetInternal x t
  = case t of
      Tip_DataSetInternal -> singleton_DataSetInternal x
merge_DataSetInternal
  = let m > r' =  m  
    in  Bin_DataSetInternal
e j l r
  = Bin_DataSetInternal 2 j Tip_DataSetInternal r
main
  = print
      $ let
          f () = T2_Main
        in h [f (), f ()]
data T_Main
  = T2_Main deriving Show
