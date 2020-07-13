{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}

module Main where
import qualified Control.DeepSeq
import qualified Control.Monad.Fail
import qualified Data.Bits
import qualified Data.Data
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Functor.Classes
import qualified Data.List
import qualified Data.Semigroup
import qualified Data.Typeable
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Enum
import qualified GHC.Err
import qualified GHC.Exts
import qualified GHC.List
import qualified GHC.Magic
import qualified GHC.Maybe
import qualified GHC.Num
import qualified GHC.Prim
import qualified GHC.Read
import qualified GHC.Show
import qualified GHC.Types
import qualified System.IO
import qualified Text.ParserCombinators.ReadPrec
import qualified Text.Read
import qualified Unsafe.Coerce
infixl 9 <&#?&<>$!#<++&<>>>
(<&#?&<>$!#<++&<>>>) ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a
  -> Set_DataSetInternal a -> Set_DataSetInternal a
m1 <&#?&<>$!#<++&<>>> m2 = difference_DataSetInternal m1 m2
{-# INLINABLE (<&#?&<>$!#<++&<>>>) #-}
data Set_DataSetInternal a
  = Bin_DataSetInternal {-# UNPACK #-} !Size_DataSetInternal !a !(Set_DataSetInternal a) !(Set_DataSetInternal a) |
    Tip_DataSetInternal
type Size_DataSetInternal = GHC.Types.Int
type role Set_DataSetInternal nominal
instance GHC.Classes.Ord a =>
         GHC.Base.Monoid (Set_DataSetInternal a) where
  mempty = empty_DataSetInternal
  mconcat = unions_DataSetInternal
  mappend = (<>)
instance GHC.Classes.Ord a =>
         GHC.Base.Semigroup (Set_DataSetInternal a) where
  (<>) = union_DataSetInternal
  stimes = Data.Semigroup.stimesIdempotentMonoid
instance Data.Foldable.Foldable Set_DataSetInternal where
  fold
    = go
    where
        go Tip_DataSetInternal = GHC.Base.mempty
        go (Bin_DataSetInternal 1 k _ _) = k
        go (Bin_DataSetInternal _ k l r)
          = go l `GHC.Base.mappend` (k `GHC.Base.mappend` go r)
  {-# INLINABLE Data.Foldable.fold #-}
  foldr = foldr_DataSetInternal
  {-# INLINE Data.Foldable.foldr #-}
  foldl = foldl_DataSetInternal
  {-# INLINE Data.Foldable.foldl #-}
  foldMap f t
    = go t
    where
        go Tip_DataSetInternal = GHC.Base.mempty
        go (Bin_DataSetInternal 1 k _ _) = f k
        go (Bin_DataSetInternal _ k l r)
          = go l `GHC.Base.mappend` (f k `GHC.Base.mappend` go r)
  {-# INLINE Data.Foldable.foldMap #-}
  foldl' = foldl'_DataSetInternal
  {-# INLINE Data.Foldable.foldl' #-}
  foldr' = foldr'_DataSetInternal
  {-# INLINE Data.Foldable.foldr' #-}
  length = size_DataSetInternal
  {-# INLINE Data.Foldable.length #-}
  null = null_DataSetInternal
  {-# INLINE Data.Foldable.null #-}
  toList = toList_DataSetInternal
  {-# INLINE Data.Foldable.toList #-}
  elem
    = go
    where
        go !_ Tip_DataSetInternal = GHC.Types.False
        go x (Bin_DataSetInternal _ y l r) = x == y || go x l || go x r
  {-# INLINABLE Data.Foldable.elem #-}
  minimum = findMin_DataSetInternal
  {-# INLINE Data.Foldable.minimum #-}
  maximum = findMax_DataSetInternal
  {-# INLINE Data.Foldable.maximum #-}
  sum = foldl'_DataSetInternal (+) 0
  {-# INLINABLE Data.Foldable.sum #-}
  product = foldl'_DataSetInternal (*) 1
  {-# INLINABLE Data.Foldable.product #-}
instance (Data.Data.Data a, GHC.Classes.Ord a) =>
         Data.Data.Data (Set_DataSetInternal a) where
  gfoldl f z set
    = z fromList_DataSetInternal `f` (toList_DataSetInternal set)
  toConstr _ = fromListConstr_DataSetInternal
  gunfold k z c
    = case Data.Data.constrIndex c of
        1 -> k (z fromList_DataSetInternal)
        _ -> GHC.Err.error "gunfold"
  dataTypeOf _ = setDataType_DataSetInternal
  dataCast1 f = Data.Typeable.gcast1 f
fromListConstr_DataSetInternal :: Data.Data.Constr
fromListConstr_DataSetInternal
  = Data.Data.mkConstr
      setDataType_DataSetInternal "fromList" [] Data.Data.Prefix
setDataType_DataSetInternal :: Data.Data.DataType
setDataType_DataSetInternal
  = Data.Data.mkDataType
      "Data.Set.Internal.Set" [fromListConstr_DataSetInternal]
null_DataSetInternal :: Set_DataSetInternal a -> GHC.Types.Bool
null_DataSetInternal Tip_DataSetInternal = GHC.Types.True
null_DataSetInternal (Bin_DataSetInternal {}) = GHC.Types.False
{-# INLINE null_DataSetInternal #-}
size_DataSetInternal :: Set_DataSetInternal a -> GHC.Types.Int
size_DataSetInternal Tip_DataSetInternal = 0
size_DataSetInternal (Bin_DataSetInternal sz _ _ _) = sz
{-# INLINE size_DataSetInternal #-}
member_DataSetInternal ::
  GHC.Classes.Ord a => a -> Set_DataSetInternal a -> GHC.Types.Bool
member_DataSetInternal
  = go
  where
      go !_ Tip_DataSetInternal = GHC.Types.False
      go x (Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT -> go x l
            GHC.Types.GT -> go x r
            GHC.Types.EQ -> GHC.Types.True
{-# INLINABLE member_DataSetInternal #-}
notMember_DataSetInternal ::
  GHC.Classes.Ord a => a -> Set_DataSetInternal a -> GHC.Types.Bool
notMember_DataSetInternal a t
  = GHC.Classes.not $ member_DataSetInternal a t
{-# INLINABLE notMember_DataSetInternal #-}
lookupLT_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> GHC.Maybe.Maybe a
lookupLT_DataSetInternal
  = goNothing
  where
      goNothing !_ Tip_DataSetInternal = GHC.Maybe.Nothing
      goNothing x (Bin_DataSetInternal _ y l r)
        | x <= y = goNothing x l
        | GHC.Base.otherwise = goJust x y r
      goJust !_ best Tip_DataSetInternal = GHC.Maybe.Just best
      goJust x best (Bin_DataSetInternal _ y l r)
        | x <= y = goJust x best l
        | GHC.Base.otherwise = goJust x y r
{-# INLINABLE lookupLT_DataSetInternal #-}
lookupGT_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> GHC.Maybe.Maybe a
lookupGT_DataSetInternal
  = goNothing
  where
      goNothing !_ Tip_DataSetInternal = GHC.Maybe.Nothing
      goNothing x (Bin_DataSetInternal _ y l r)
        | x < y = goJust x y l
        | GHC.Base.otherwise = goNothing x r
      goJust !_ best Tip_DataSetInternal = GHC.Maybe.Just best
      goJust x best (Bin_DataSetInternal _ y l r)
        | x < y = goJust x y l
        | GHC.Base.otherwise = goJust x best r
{-# INLINABLE lookupGT_DataSetInternal #-}
lookupLE_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> GHC.Maybe.Maybe a
lookupLE_DataSetInternal
  = goNothing
  where
      goNothing !_ Tip_DataSetInternal = GHC.Maybe.Nothing
      goNothing x (Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT -> goNothing x l
            GHC.Types.EQ -> GHC.Maybe.Just y
            GHC.Types.GT -> goJust x y r
      goJust !_ best Tip_DataSetInternal = GHC.Maybe.Just best
      goJust x best (Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT -> goJust x best l
            GHC.Types.EQ -> GHC.Maybe.Just y
            GHC.Types.GT -> goJust x y r
{-# INLINABLE lookupLE_DataSetInternal #-}
lookupGE_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> GHC.Maybe.Maybe a
lookupGE_DataSetInternal
  = goNothing
  where
      goNothing !_ Tip_DataSetInternal = GHC.Maybe.Nothing
      goNothing x (Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT -> goJust x y l
            GHC.Types.EQ -> GHC.Maybe.Just y
            GHC.Types.GT -> goNothing x r
      goJust !_ best Tip_DataSetInternal = GHC.Maybe.Just best
      goJust x best (Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT -> goJust x y l
            GHC.Types.EQ -> GHC.Maybe.Just y
            GHC.Types.GT -> goJust x best r
{-# INLINABLE lookupGE_DataSetInternal #-}
empty_DataSetInternal :: Set_DataSetInternal a
empty_DataSetInternal = Tip_DataSetInternal
{-# INLINE empty_DataSetInternal #-}
singleton_DataSetInternal :: a -> Set_DataSetInternal a
singleton_DataSetInternal x
  = Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
{-# INLINE singleton_DataSetInternal #-}
insert_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> Set_DataSetInternal a
insert_DataSetInternal x0
  = go x0 x0
  where
      go ::
        GHC.Classes.Ord a =>
        a -> a -> Set_DataSetInternal a -> Set_DataSetInternal a
      go orig !_ Tip_DataSetInternal
        = singleton_DataSetInternal (GHC.Magic.lazy orig)
      go orig !x t@(Bin_DataSetInternal sz y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT
              | l' `ptrEq_UtilsContainersInternalPtrEquality` l -> t
              | GHC.Base.otherwise -> balanceL_DataSetInternal y l' r
              where
                  !l' = go orig x l
            GHC.Types.GT
              | r' `ptrEq_UtilsContainersInternalPtrEquality` r -> t
              | GHC.Base.otherwise -> balanceR_DataSetInternal y l r'
              where
                  !r' = go orig x r
            GHC.Types.EQ
              | GHC.Magic.lazy orig
                  `GHC.Prim.seq` (orig `ptrEq_UtilsContainersInternalPtrEquality` y)
              -> t
              | GHC.Base.otherwise
              -> Bin_DataSetInternal sz (GHC.Magic.lazy orig) l r
{-# INLINABLE insert_DataSetInternal #-}
insertR_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> Set_DataSetInternal a
insertR_DataSetInternal x0
  = go x0 x0
  where
      go ::
        GHC.Classes.Ord a =>
        a -> a -> Set_DataSetInternal a -> Set_DataSetInternal a
      go orig !_ Tip_DataSetInternal
        = singleton_DataSetInternal (GHC.Magic.lazy orig)
      go orig !x t@(Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT
              | l' `ptrEq_UtilsContainersInternalPtrEquality` l -> t
              | GHC.Base.otherwise -> balanceL_DataSetInternal y l' r
              where
                  !l' = go orig x l
            GHC.Types.GT
              | r' `ptrEq_UtilsContainersInternalPtrEquality` r -> t
              | GHC.Base.otherwise -> balanceR_DataSetInternal y l r'
              where
                  !r' = go orig x r
            GHC.Types.EQ -> t
{-# INLINABLE insertR_DataSetInternal #-}
delete_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> Set_DataSetInternal a
delete_DataSetInternal
  = go
  where
      go ::
        GHC.Classes.Ord a =>
        a -> Set_DataSetInternal a -> Set_DataSetInternal a
      go !_ Tip_DataSetInternal = Tip_DataSetInternal
      go x t@(Bin_DataSetInternal _ y l r)
        = case GHC.Classes.compare x y of
            GHC.Types.LT
              | l' `ptrEq_UtilsContainersInternalPtrEquality` l -> t
              | GHC.Base.otherwise -> balanceR_DataSetInternal y l' r
              where
                  !l' = go x l
            GHC.Types.GT
              | r' `ptrEq_UtilsContainersInternalPtrEquality` r -> t
              | GHC.Base.otherwise -> balanceL_DataSetInternal y l r'
              where
                  !r' = go x r
            GHC.Types.EQ -> glue_DataSetInternal l r
{-# INLINABLE delete_DataSetInternal #-}
isProperSubsetOf_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a -> Set_DataSetInternal a -> GHC.Types.Bool
isProperSubsetOf_DataSetInternal s1 s2
  = (size_DataSetInternal s1 < size_DataSetInternal s2)
      && (isSubsetOf_DataSetInternal s1 s2)
{-# INLINABLE isProperSubsetOf_DataSetInternal #-}
isSubsetOf_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a -> Set_DataSetInternal a -> GHC.Types.Bool
isSubsetOf_DataSetInternal t1 t2
  = (size_DataSetInternal t1 <= size_DataSetInternal t2)
      && (isSubsetOfX_DataSetInternal t1 t2)
{-# INLINABLE isSubsetOf_DataSetInternal #-}
isSubsetOfX_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a -> Set_DataSetInternal a -> GHC.Types.Bool
isSubsetOfX_DataSetInternal Tip_DataSetInternal _ = GHC.Types.True
isSubsetOfX_DataSetInternal _ Tip_DataSetInternal = GHC.Types.False
isSubsetOfX_DataSetInternal (Bin_DataSetInternal _ x l r) t
  = found && isSubsetOfX_DataSetInternal l lt
      && isSubsetOfX_DataSetInternal r gt
  where
      (lt, found, gt) = splitMember_DataSetInternal x t
{-# INLINABLE isSubsetOfX_DataSetInternal #-}
disjoint_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a -> Set_DataSetInternal a -> GHC.Types.Bool
disjoint_DataSetInternal Tip_DataSetInternal _ = GHC.Types.True
disjoint_DataSetInternal _ Tip_DataSetInternal = GHC.Types.True
disjoint_DataSetInternal (Bin_DataSetInternal _ x l r) t
  = GHC.Classes.not found && disjoint_DataSetInternal l lt
      && disjoint_DataSetInternal r gt
  where
      (lt, found, gt) = splitMember_DataSetInternal x t
lookupMinSure_DataSetInternal :: a -> Set_DataSetInternal a -> a
lookupMinSure_DataSetInternal x Tip_DataSetInternal = x
lookupMinSure_DataSetInternal _ (Bin_DataSetInternal _ x l _)
  = lookupMinSure_DataSetInternal x l
lookupMin_DataSetInternal ::
  Set_DataSetInternal a -> GHC.Maybe.Maybe a
lookupMin_DataSetInternal Tip_DataSetInternal = GHC.Maybe.Nothing
lookupMin_DataSetInternal (Bin_DataSetInternal _ x l _)
  = GHC.Maybe.Just $! lookupMinSure_DataSetInternal x l
findMin_DataSetInternal :: Set_DataSetInternal a -> a
findMin_DataSetInternal t
  | GHC.Maybe.Just r <- lookupMin_DataSetInternal t = r
  | GHC.Base.otherwise
  = GHC.Err.error "Set.findMin: empty set has no minimal element"
lookupMaxSure_DataSetInternal :: a -> Set_DataSetInternal a -> a
lookupMaxSure_DataSetInternal x Tip_DataSetInternal = x
lookupMaxSure_DataSetInternal _ (Bin_DataSetInternal _ x _ r)
  = lookupMaxSure_DataSetInternal x r
lookupMax_DataSetInternal ::
  Set_DataSetInternal a -> GHC.Maybe.Maybe a
lookupMax_DataSetInternal Tip_DataSetInternal = GHC.Maybe.Nothing
lookupMax_DataSetInternal (Bin_DataSetInternal _ x _ r)
  = GHC.Maybe.Just $! lookupMaxSure_DataSetInternal x r
findMax_DataSetInternal :: Set_DataSetInternal a -> a
findMax_DataSetInternal t
  | GHC.Maybe.Just r <- lookupMax_DataSetInternal t = r
  | GHC.Base.otherwise
  = GHC.Err.error "Set.findMax: empty set has no maximal element"
deleteMin_DataSetInternal ::
  Set_DataSetInternal a -> Set_DataSetInternal a
deleteMin_DataSetInternal
  (Bin_DataSetInternal _ _ Tip_DataSetInternal r)
  = r
deleteMin_DataSetInternal (Bin_DataSetInternal _ x l r)
  = balanceR_DataSetInternal x (deleteMin_DataSetInternal l) r
deleteMin_DataSetInternal Tip_DataSetInternal = Tip_DataSetInternal
deleteMax_DataSetInternal ::
  Set_DataSetInternal a -> Set_DataSetInternal a
deleteMax_DataSetInternal
  (Bin_DataSetInternal _ _ l Tip_DataSetInternal)
  = l
deleteMax_DataSetInternal (Bin_DataSetInternal _ x l r)
  = balanceL_DataSetInternal x l (deleteMax_DataSetInternal r)
deleteMax_DataSetInternal Tip_DataSetInternal = Tip_DataSetInternal
unions_DataSetInternal ::
  (Data.Foldable.Foldable f, GHC.Classes.Ord a) =>
  f (Set_DataSetInternal a) -> Set_DataSetInternal a
unions_DataSetInternal
  = Data.Foldable.foldl' union_DataSetInternal empty_DataSetInternal
{-# INLINABLE unions_DataSetInternal #-}
union_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a
  -> Set_DataSetInternal a -> Set_DataSetInternal a
union_DataSetInternal t1 Tip_DataSetInternal = t1
union_DataSetInternal t1 (Bin_DataSetInternal 1 x _ _)
  = insertR_DataSetInternal x t1
union_DataSetInternal (Bin_DataSetInternal 1 x _ _) t2
  = insert_DataSetInternal x t2
union_DataSetInternal Tip_DataSetInternal t2 = t2
union_DataSetInternal t1@(Bin_DataSetInternal _ x l1 r1) t2
  = case splitS_DataSetInternal x t2 of {
      (l2 :*: r2)
        | l1l2 `ptrEq_UtilsContainersInternalPtrEquality` l1 && r1r2
            `ptrEq_UtilsContainersInternalPtrEquality` r1
        -> t1
        | GHC.Base.otherwise -> link_DataSetInternal x l1l2 r1r2
        where
            !l1l2 = union_DataSetInternal l1 l2
            !r1r2 = union_DataSetInternal r1 r2 }
{-# INLINABLE union_DataSetInternal #-}
difference_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a
  -> Set_DataSetInternal a -> Set_DataSetInternal a
difference_DataSetInternal Tip_DataSetInternal _
  = Tip_DataSetInternal
difference_DataSetInternal t1 Tip_DataSetInternal = t1
difference_DataSetInternal t1 (Bin_DataSetInternal _ x l2 r2)
  = case split_DataSetInternal x t1 of {
      (l1, r1)
        | size_DataSetInternal l1l2 + size_DataSetInternal r1r2
            == size_DataSetInternal t1
        -> t1
        | GHC.Base.otherwise -> merge_DataSetInternal l1l2 r1r2
        where
            !l1l2 = difference_DataSetInternal l1 l2
            !r1r2 = difference_DataSetInternal r1 r2 }
{-# INLINABLE difference_DataSetInternal #-}
intersection_DataSetInternal ::
  GHC.Classes.Ord a =>
  Set_DataSetInternal a
  -> Set_DataSetInternal a -> Set_DataSetInternal a
intersection_DataSetInternal Tip_DataSetInternal _
  = Tip_DataSetInternal
intersection_DataSetInternal _ Tip_DataSetInternal
  = Tip_DataSetInternal
intersection_DataSetInternal t1@(Bin_DataSetInternal _ x l1 r1) t2
  | b
  = if l1l2 `ptrEq_UtilsContainersInternalPtrEquality` l1 && r1r2
         `ptrEq_UtilsContainersInternalPtrEquality` r1 then
        t1
    else
        link_DataSetInternal x l1l2 r1r2
  | GHC.Base.otherwise = merge_DataSetInternal l1l2 r1r2
  where
      !(l2, b, r2) = splitMember_DataSetInternal x t2
      !l1l2 = intersection_DataSetInternal l1 l2
      !r1r2 = intersection_DataSetInternal r1 r2
{-# INLINABLE intersection_DataSetInternal #-}
filter_DataSetInternal ::
  (a -> GHC.Types.Bool)
  -> Set_DataSetInternal a -> Set_DataSetInternal a
filter_DataSetInternal _ Tip_DataSetInternal = Tip_DataSetInternal
filter_DataSetInternal p t@(Bin_DataSetInternal _ x l r)
  | p x
  = if l `ptrEq_UtilsContainersInternalPtrEquality` l' && r
         `ptrEq_UtilsContainersInternalPtrEquality` r' then
        t
    else
        link_DataSetInternal x l' r'
  | GHC.Base.otherwise = merge_DataSetInternal l' r'
  where
      !l' = filter_DataSetInternal p l
      !r' = filter_DataSetInternal p r
partition_DataSetInternal ::
  (a -> GHC.Types.Bool)
  -> Set_DataSetInternal a
     -> (Set_DataSetInternal a, Set_DataSetInternal a)
partition_DataSetInternal p0 t0
  = toPair_UtilsContainersInternalStrictPair $ go p0 t0
  where
      go _ Tip_DataSetInternal
        = (Tip_DataSetInternal :*: Tip_DataSetInternal)
      go p t@(Bin_DataSetInternal _ x l r)
        = case (go p l, go p r) of {
            ((l1 :*: l2), (r1 :*: r2))
              | p x
              -> (if l1 `ptrEq_UtilsContainersInternalPtrEquality` l && r1
                       `ptrEq_UtilsContainersInternalPtrEquality` r then
                      t
                  else
                      link_DataSetInternal x l1 r1)
                   :*: merge_DataSetInternal l2 r2
              | GHC.Base.otherwise
              -> merge_DataSetInternal l1 r1
                   :*:
                     (if l2 `ptrEq_UtilsContainersInternalPtrEquality` l && r2
                           `ptrEq_UtilsContainersInternalPtrEquality` r then
                          t
                      else
                          link_DataSetInternal x l2 r2) }
map_DataSetInternal ::
  GHC.Classes.Ord b =>
  (a -> b) -> Set_DataSetInternal a -> Set_DataSetInternal b
map_DataSetInternal f
  = fromList_DataSetInternal . GHC.Base.map f
      . toList_DataSetInternal
{-# INLINABLE map_DataSetInternal #-}
mapMonotonic_DataSetInternal ::
  (a -> b) -> Set_DataSetInternal a -> Set_DataSetInternal b
mapMonotonic_DataSetInternal _ Tip_DataSetInternal
  = Tip_DataSetInternal
mapMonotonic_DataSetInternal f (Bin_DataSetInternal sz x l r)
  = Bin_DataSetInternal
      sz
      (f x)
      (mapMonotonic_DataSetInternal f l)
      (mapMonotonic_DataSetInternal f r)
fold_DataSetInternal ::
  (a -> b -> b) -> b -> Set_DataSetInternal a -> b
fold_DataSetInternal = foldr_DataSetInternal
{-# INLINE fold_DataSetInternal #-}
foldr_DataSetInternal ::
  (a -> b -> b) -> b -> Set_DataSetInternal a -> b
foldr_DataSetInternal f z
  = go z
  where
      go z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr_DataSetInternal #-}
foldr'_DataSetInternal ::
  (a -> b -> b) -> b -> Set_DataSetInternal a -> b
foldr'_DataSetInternal f z
  = go z
  where
      go !z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal _ x l r) = go (f x (go z' r)) l
{-# INLINE foldr'_DataSetInternal #-}
foldl_DataSetInternal ::
  (a -> b -> a) -> a -> Set_DataSetInternal b -> a
foldl_DataSetInternal f z
  = go z
  where
      go z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl_DataSetInternal #-}
foldl'_DataSetInternal ::
  (a -> b -> a) -> a -> Set_DataSetInternal b -> a
foldl'_DataSetInternal f z
  = go z
  where
      go !z' Tip_DataSetInternal = z'
      go z' (Bin_DataSetInternal _ x l r) = go (f (go z' l) x) r
{-# INLINE foldl'_DataSetInternal #-}
elems_DataSetInternal :: Set_DataSetInternal a -> [a]
elems_DataSetInternal = toAscList_DataSetInternal
instance (GHC.Classes.Ord a) =>
         GHC.Exts.IsList (Set_DataSetInternal a) where
  type Item (Set_DataSetInternal a) = a
  fromList = fromList_DataSetInternal
  toList = toList_DataSetInternal
toList_DataSetInternal :: Set_DataSetInternal a -> [a]
toList_DataSetInternal = toAscList_DataSetInternal
toAscList_DataSetInternal :: Set_DataSetInternal a -> [a]
toAscList_DataSetInternal = foldr_DataSetInternal (:) []
toDescList_DataSetInternal :: Set_DataSetInternal a -> [a]
toDescList_DataSetInternal
  = foldl_DataSetInternal (GHC.Base.flip (:)) []
foldrFB_DataSetInternal ::
  (a -> b -> b) -> b -> Set_DataSetInternal a -> b
foldrFB_DataSetInternal = foldr_DataSetInternal
{-# INLINE [0] foldrFB_DataSetInternal #-}
foldlFB_DataSetInternal ::
  (a -> b -> a) -> a -> Set_DataSetInternal b -> a
foldlFB_DataSetInternal = foldl_DataSetInternal
{-# INLINE [0] foldlFB_DataSetInternal #-}
{-# INLINE elems_DataSetInternal #-}
{-# INLINE toList_DataSetInternal #-}
{-# NOINLINE [0] toAscList_DataSetInternal #-}
{-# NOINLINE [0] toDescList_DataSetInternal #-}
{-# RULES "Set.toAscList" [~1]
              forall s. toAscList_DataSetInternal s
                = GHC.Base.build (\ c n -> foldrFB_DataSetInternal c n s) #-}
{-# RULES "Set.toAscListBack" [1]
              foldrFB_DataSetInternal (:) []
                = toAscList_DataSetInternal #-}
{-# RULES "Set.toDescList" [~1]
              forall s. toDescList_DataSetInternal s
                = GHC.Base.build
                    (\ c n -> foldlFB_DataSetInternal (\ xs x -> c x xs) n s) #-}
{-# RULES "Set.toDescListBack" [1]
              foldlFB_DataSetInternal (\ xs x -> x : xs) []
                = toDescList_DataSetInternal #-}
fromList_DataSetInternal ::
  GHC.Classes.Ord a => [a] -> Set_DataSetInternal a
fromList_DataSetInternal [] = Tip_DataSetInternal
fromList_DataSetInternal [x]
  = Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
fromList_DataSetInternal (x0 : xs0)
  | not_ordered x0 xs0
  = fromList'
      (Bin_DataSetInternal 1 x0 Tip_DataSetInternal Tip_DataSetInternal)
      xs0
  | GHC.Base.otherwise
  = go
      (1 :: GHC.Types.Int)
      (Bin_DataSetInternal 1 x0 Tip_DataSetInternal Tip_DataSetInternal)
      xs0
  where
      not_ordered _ [] = GHC.Types.False
      not_ordered x (y : _) = x >= y
      {-# INLINE not_ordered #-}
      fromList' t0 xs
        = Data.Foldable.foldl' ins t0 xs
        where
            ins t x = insert_DataSetInternal x t
      go !_ t [] = t
      go _ t [x] = insertMax_DataSetInternal x t
      go s l xs@(x : xss)
        | not_ordered x xss = fromList' l xs
        | GHC.Base.otherwise
        = case create s xss of
            (r, ys, [])
              -> go (s `Data.Bits.shiftL` 1) (link_DataSetInternal x l r) ys
            (r, _, ys) -> fromList' (link_DataSetInternal x l r) ys
      create !_ [] = (Tip_DataSetInternal, [], [])
      create s xs@(x : xss)
        | s == 1
        = if not_ordered x xss then
              (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal, 
               [], xss)
          else
              (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal, 
               xss, [])
        | GHC.Base.otherwise
        = case create (s `Data.Bits.shiftR` 1) xs of
            res@(_, [], _) -> res
            (l, [y], zs) -> (insertMax_DataSetInternal y l, [], zs)
            (l, ys@(y : yss), _)
              | not_ordered y yss -> (l, [], ys)
              | GHC.Base.otherwise
              -> case create (s `Data.Bits.shiftR` 1) yss of {
                   (r, zs, ws) -> (link_DataSetInternal y l r, zs, ws) }
{-# INLINABLE fromList_DataSetInternal #-}
fromAscList_DataSetInternal ::
  GHC.Classes.Eq a => [a] -> Set_DataSetInternal a
fromAscList_DataSetInternal xs
  = fromDistinctAscList_DataSetInternal
      (combineEq_DataSetInternal xs)
{-# INLINABLE fromAscList_DataSetInternal #-}
fromDescList_DataSetInternal ::
  GHC.Classes.Eq a => [a] -> Set_DataSetInternal a
fromDescList_DataSetInternal xs
  = fromDistinctDescList_DataSetInternal
      (combineEq_DataSetInternal xs)
{-# INLINABLE fromDescList_DataSetInternal #-}
combineEq_DataSetInternal :: GHC.Classes.Eq a => [a] -> [a]
combineEq_DataSetInternal [] = []
combineEq_DataSetInternal (x : xs)
  = combineEq' x xs
  where
      combineEq' z [] = [z]
      combineEq' z (y : ys)
        | z == y = combineEq' z ys
        | GHC.Base.otherwise = z : combineEq' y ys
fromDistinctAscList_DataSetInternal :: [a] -> Set_DataSetInternal a
fromDistinctAscList_DataSetInternal [] = Tip_DataSetInternal
fromDistinctAscList_DataSetInternal (x0 : xs0)
  = go
      (1 :: GHC.Types.Int)
      (Bin_DataSetInternal 1 x0 Tip_DataSetInternal Tip_DataSetInternal)
      xs0
  where
      go !_ t [] = t
      go s l (x : xs)
        = case create s xs of {
            (r :*: ys)
              -> let !t' = link_DataSetInternal x l r
                 in go (s `Data.Bits.shiftL` 1) t' ys }
      create !_ [] = (Tip_DataSetInternal :*: [])
      create s xs@(x : xs')
        | s == 1
        = (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
             :*: xs')
        | GHC.Base.otherwise
        = case create (s `Data.Bits.shiftR` 1) xs of
            res@(_ :*: []) -> res
            (l :*: (y : ys))
              -> case create (s `Data.Bits.shiftR` 1) ys of {
                   (r :*: zs) -> (link_DataSetInternal y l r :*: zs) }
fromDistinctDescList_DataSetInternal ::
  [a] -> Set_DataSetInternal a
fromDistinctDescList_DataSetInternal [] = Tip_DataSetInternal
fromDistinctDescList_DataSetInternal (x0 : xs0)
  = go
      (1 :: GHC.Types.Int)
      (Bin_DataSetInternal 1 x0 Tip_DataSetInternal Tip_DataSetInternal)
      xs0
  where
      go !_ t [] = t
      go s r (x : xs)
        = case create s xs of {
            (l :*: ys)
              -> let !t' = link_DataSetInternal x l r
                 in go (s `Data.Bits.shiftL` 1) t' ys }
      create !_ [] = (Tip_DataSetInternal :*: [])
      create s xs@(x : xs')
        | s == 1
        = (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
             :*: xs')
        | GHC.Base.otherwise
        = case create (s `Data.Bits.shiftR` 1) xs of
            res@(_ :*: []) -> res
            (r :*: (y : ys))
              -> case create (s `Data.Bits.shiftR` 1) ys of {
                   (l :*: zs) -> (link_DataSetInternal y l r :*: zs) }
instance GHC.Classes.Eq a =>
         GHC.Classes.Eq (Set_DataSetInternal a) where
  t1 == t2
    = (size_DataSetInternal t1 == size_DataSetInternal t2)
        && (toAscList_DataSetInternal t1 == toAscList_DataSetInternal t2)
instance GHC.Classes.Ord a =>
         GHC.Classes.Ord (Set_DataSetInternal a) where
  compare s1 s2
    = GHC.Classes.compare
        (toAscList_DataSetInternal s1) (toAscList_DataSetInternal s2)
instance GHC.Show.Show a =>
         GHC.Show.Show (Set_DataSetInternal a) where
  showsPrec p xs
    = GHC.Show.showParen (p > 10) $ GHC.Show.showString "fromList "
        . GHC.Show.shows (toList_DataSetInternal xs)
instance Data.Functor.Classes.Eq1 Set_DataSetInternal where
  liftEq eq m n
    = size_DataSetInternal m == size_DataSetInternal n
        &&
          Data.Functor.Classes.liftEq
            eq (toList_DataSetInternal m) (toList_DataSetInternal n)
instance Data.Functor.Classes.Ord1 Set_DataSetInternal where
  liftCompare cmp m n
    = Data.Functor.Classes.liftCompare
        cmp (toList_DataSetInternal m) (toList_DataSetInternal n)
instance Data.Functor.Classes.Show1 Set_DataSetInternal where
  liftShowsPrec sp sl d m
    = Data.Functor.Classes.showsUnaryWith
        (Data.Functor.Classes.liftShowsPrec sp sl)
        "fromList"
        d
        (toList_DataSetInternal m)
instance (GHC.Read.Read a, GHC.Classes.Ord a) =>
         GHC.Read.Read (Set_DataSetInternal a) where
  readPrec
    = GHC.Read.parens $ Text.ParserCombinators.ReadPrec.prec 10
        $ do Text.Read.Ident "fromList" <- GHC.Read.lexP
             xs <- GHC.Read.readPrec
             GHC.Base.return (fromList_DataSetInternal xs)
  readListPrec = GHC.Read.readListPrecDefault
deriving instance Data.Typeable.Typeable Set_DataSetInternal
instance Control.DeepSeq.NFData a =>
         Control.DeepSeq.NFData (Set_DataSetInternal a) where
  rnf Tip_DataSetInternal = ()
  rnf (Bin_DataSetInternal _ y l r)
    = Control.DeepSeq.rnf y `GHC.Prim.seq` Control.DeepSeq.rnf l
        `GHC.Prim.seq` Control.DeepSeq.rnf r
split_DataSetInternal ::
  GHC.Classes.Ord a =>
  a
  -> Set_DataSetInternal a
     -> (Set_DataSetInternal a, Set_DataSetInternal a)
split_DataSetInternal x t
  = toPair_UtilsContainersInternalStrictPair
      $ splitS_DataSetInternal x t
{-# INLINABLE split_DataSetInternal #-}
splitS_DataSetInternal ::
  GHC.Classes.Ord a =>
  a
  -> Set_DataSetInternal a
     -> StrictPair_UtilsContainersInternalStrictPair (Set_DataSetInternal a) (Set_DataSetInternal a)
splitS_DataSetInternal _ Tip_DataSetInternal
  = (Tip_DataSetInternal :*: Tip_DataSetInternal)
splitS_DataSetInternal x (Bin_DataSetInternal _ y l r)
  = case GHC.Classes.compare x y of
      GHC.Types.LT
        -> let (lt :*: gt) = splitS_DataSetInternal x l
           in (lt :*: link_DataSetInternal y gt r)
      GHC.Types.GT
        -> let (lt :*: gt) = splitS_DataSetInternal x r
           in (link_DataSetInternal y l lt :*: gt)
      GHC.Types.EQ -> (l :*: r)
{-# INLINABLE splitS_DataSetInternal #-}
splitMember_DataSetInternal ::
  GHC.Classes.Ord a =>
  a
  -> Set_DataSetInternal a
     -> (Set_DataSetInternal a, GHC.Types.Bool, Set_DataSetInternal a)
splitMember_DataSetInternal _ Tip_DataSetInternal
  = (Tip_DataSetInternal, GHC.Types.False, Tip_DataSetInternal)
splitMember_DataSetInternal x (Bin_DataSetInternal _ y l r)
  = case GHC.Classes.compare x y of
      GHC.Types.LT
        -> let
             (lt, found, gt) = splitMember_DataSetInternal x l
             !gt' = link_DataSetInternal y gt r
           in (lt, found, gt')
      GHC.Types.GT
        -> let
             (lt, found, gt) = splitMember_DataSetInternal x r
             !lt' = link_DataSetInternal y l lt
           in (lt', found, gt)
      GHC.Types.EQ -> (l, GHC.Types.True, r)
{-# INLINABLE splitMember_DataSetInternal #-}
findIndex_DataSetInternal ::
  GHC.Classes.Ord a => a -> Set_DataSetInternal a -> GHC.Types.Int
findIndex_DataSetInternal
  = go 0
  where
      go ::
        GHC.Classes.Ord a =>
        GHC.Types.Int -> a -> Set_DataSetInternal a -> GHC.Types.Int
      go !_ !_ Tip_DataSetInternal
        = GHC.Err.error "Set.findIndex: element is not in the set"
      go idx x (Bin_DataSetInternal _ kx l r)
        = case GHC.Classes.compare x kx of
            GHC.Types.LT -> go idx x l
            GHC.Types.GT -> go (idx + size_DataSetInternal l + 1) x r
            GHC.Types.EQ -> idx + size_DataSetInternal l
{-# INLINABLE findIndex_DataSetInternal #-}
lookupIndex_DataSetInternal ::
  GHC.Classes.Ord a =>
  a -> Set_DataSetInternal a -> GHC.Maybe.Maybe GHC.Types.Int
lookupIndex_DataSetInternal
  = go 0
  where
      go ::
        GHC.Classes.Ord a =>
        GHC.Types.Int
        -> a -> Set_DataSetInternal a -> GHC.Maybe.Maybe GHC.Types.Int
      go !_ !_ Tip_DataSetInternal = GHC.Maybe.Nothing
      go idx x (Bin_DataSetInternal _ kx l r)
        = case GHC.Classes.compare x kx of
            GHC.Types.LT -> go idx x l
            GHC.Types.GT -> go (idx + size_DataSetInternal l + 1) x r
            GHC.Types.EQ -> GHC.Maybe.Just $! idx + size_DataSetInternal l
{-# INLINABLE lookupIndex_DataSetInternal #-}
elemAt_DataSetInternal ::
  GHC.Types.Int -> Set_DataSetInternal a -> a
elemAt_DataSetInternal !_ Tip_DataSetInternal
  = GHC.Err.error "Set.elemAt: index out of range"
elemAt_DataSetInternal i (Bin_DataSetInternal _ x l r)
  = case GHC.Classes.compare i sizeL of
      GHC.Types.LT -> elemAt_DataSetInternal i l
      GHC.Types.GT -> elemAt_DataSetInternal (i - sizeL - 1) r
      GHC.Types.EQ -> x
  where
      sizeL = size_DataSetInternal l
deleteAt_DataSetInternal ::
  GHC.Types.Int -> Set_DataSetInternal a -> Set_DataSetInternal a
deleteAt_DataSetInternal !i t
  = case t of
      Tip_DataSetInternal
        -> GHC.Err.error "Set.deleteAt: index out of range"
      Bin_DataSetInternal _ x l r
        -> case GHC.Classes.compare i sizeL of
             GHC.Types.LT
               -> balanceR_DataSetInternal x (deleteAt_DataSetInternal i l) r
             GHC.Types.GT
               -> balanceL_DataSetInternal
                    x l (deleteAt_DataSetInternal (i - sizeL - 1) r)
             GHC.Types.EQ -> glue_DataSetInternal l r
        where
            sizeL = size_DataSetInternal l
take_DataSetInternal ::
  GHC.Types.Int -> Set_DataSetInternal a -> Set_DataSetInternal a
take_DataSetInternal i m | i >= size_DataSetInternal m = m
take_DataSetInternal i0 m0
  = go i0 m0
  where
      go i !_ | i <= 0 = Tip_DataSetInternal
      go !_ Tip_DataSetInternal = Tip_DataSetInternal
      go i (Bin_DataSetInternal _ x l r)
        = case GHC.Classes.compare i sizeL of
            GHC.Types.LT -> go i l
            GHC.Types.GT -> link_DataSetInternal x l (go (i - sizeL - 1) r)
            GHC.Types.EQ -> l
        where
            sizeL = size_DataSetInternal l
drop_DataSetInternal ::
  GHC.Types.Int -> Set_DataSetInternal a -> Set_DataSetInternal a
drop_DataSetInternal i m
  | i >= size_DataSetInternal m = Tip_DataSetInternal
drop_DataSetInternal i0 m0
  = go i0 m0
  where
      go i m | i <= 0 = m
      go !_ Tip_DataSetInternal = Tip_DataSetInternal
      go i (Bin_DataSetInternal _ x l r)
        = case GHC.Classes.compare i sizeL of
            GHC.Types.LT -> link_DataSetInternal x (go i l) r
            GHC.Types.GT -> go (i - sizeL - 1) r
            GHC.Types.EQ -> insertMin_DataSetInternal x r
        where
            sizeL = size_DataSetInternal l
splitAt_DataSetInternal ::
  GHC.Types.Int
  -> Set_DataSetInternal a
     -> (Set_DataSetInternal a, Set_DataSetInternal a)
splitAt_DataSetInternal i0 m0
  | i0 >= size_DataSetInternal m0 = (m0, Tip_DataSetInternal)
  | GHC.Base.otherwise
  = toPair_UtilsContainersInternalStrictPair $ go i0 m0
  where
      go i m | i <= 0 = Tip_DataSetInternal :*: m
      go !_ Tip_DataSetInternal
        = Tip_DataSetInternal :*: Tip_DataSetInternal
      go i (Bin_DataSetInternal _ x l r)
        = case GHC.Classes.compare i sizeL of
            GHC.Types.LT
              -> case go i l of {
                   ll :*: lr -> ll :*: link_DataSetInternal x lr r }
            GHC.Types.GT
              -> case go (i - sizeL - 1) r of {
                   rl :*: rr -> link_DataSetInternal x l rl :*: rr }
            GHC.Types.EQ -> l :*: insertMin_DataSetInternal x r
        where
            sizeL = size_DataSetInternal l
takeWhileAntitone_DataSetInternal ::
  (a -> GHC.Types.Bool)
  -> Set_DataSetInternal a -> Set_DataSetInternal a
takeWhileAntitone_DataSetInternal _ Tip_DataSetInternal
  = Tip_DataSetInternal
takeWhileAntitone_DataSetInternal p (Bin_DataSetInternal _ x l r)
  | p x
  = link_DataSetInternal x l (takeWhileAntitone_DataSetInternal p r)
  | GHC.Base.otherwise = takeWhileAntitone_DataSetInternal p l
dropWhileAntitone_DataSetInternal ::
  (a -> GHC.Types.Bool)
  -> Set_DataSetInternal a -> Set_DataSetInternal a
dropWhileAntitone_DataSetInternal _ Tip_DataSetInternal
  = Tip_DataSetInternal
dropWhileAntitone_DataSetInternal p (Bin_DataSetInternal _ x l r)
  | p x = dropWhileAntitone_DataSetInternal p r
  | GHC.Base.otherwise
  = link_DataSetInternal x (dropWhileAntitone_DataSetInternal p l) r
spanAntitone_DataSetInternal ::
  (a -> GHC.Types.Bool)
  -> Set_DataSetInternal a
     -> (Set_DataSetInternal a, Set_DataSetInternal a)
spanAntitone_DataSetInternal p0 m
  = toPair_UtilsContainersInternalStrictPair (go p0 m)
  where
      go _ Tip_DataSetInternal
        = Tip_DataSetInternal :*: Tip_DataSetInternal
      go p (Bin_DataSetInternal _ x l r)
        | p x = let u :*: v = go p r in link_DataSetInternal x l u :*: v
        | GHC.Base.otherwise
        = let u :*: v = go p l in u :*: link_DataSetInternal x v r
link_DataSetInternal ::
  a
  -> Set_DataSetInternal a
     -> Set_DataSetInternal a -> Set_DataSetInternal a
link_DataSetInternal x Tip_DataSetInternal r
  = insertMin_DataSetInternal x r
link_DataSetInternal x l Tip_DataSetInternal
  = insertMax_DataSetInternal x l
link_DataSetInternal
  x
  l@(Bin_DataSetInternal sizeL y ly ry)
  r@(Bin_DataSetInternal sizeR z lz rz)
  | delta_DataSetInternal * sizeL < sizeR
  = balanceL_DataSetInternal z (link_DataSetInternal x l lz) rz
  | delta_DataSetInternal * sizeR < sizeL
  = balanceR_DataSetInternal y ly (link_DataSetInternal x ry r)
  | GHC.Base.otherwise = bin_DataSetInternal x l r
insertMax_DataSetInternal, insertMin_DataSetInternal ::
  a -> Set_DataSetInternal a -> Set_DataSetInternal a
insertMax_DataSetInternal x t
  = case t of
      Tip_DataSetInternal -> singleton_DataSetInternal x
      Bin_DataSetInternal _ y l r
        -> balanceR_DataSetInternal y l (insertMax_DataSetInternal x r)
insertMin_DataSetInternal x t
  = case t of
      Tip_DataSetInternal -> singleton_DataSetInternal x
      Bin_DataSetInternal _ y l r
        -> balanceL_DataSetInternal y (insertMin_DataSetInternal x l) r
merge_DataSetInternal ::
  Set_DataSetInternal a
  -> Set_DataSetInternal a -> Set_DataSetInternal a
merge_DataSetInternal Tip_DataSetInternal r = r
merge_DataSetInternal l Tip_DataSetInternal = l
merge_DataSetInternal
  l@(Bin_DataSetInternal sizeL x lx rx)
  r@(Bin_DataSetInternal sizeR y ly ry)
  | delta_DataSetInternal * sizeL < sizeR
  = balanceL_DataSetInternal y (merge_DataSetInternal l ly) ry
  | delta_DataSetInternal * sizeR < sizeL
  = balanceR_DataSetInternal x lx (merge_DataSetInternal rx r)
  | GHC.Base.otherwise = glue_DataSetInternal l r
glue_DataSetInternal ::
  Set_DataSetInternal a
  -> Set_DataSetInternal a -> Set_DataSetInternal a
glue_DataSetInternal Tip_DataSetInternal r = r
glue_DataSetInternal l Tip_DataSetInternal = l
glue_DataSetInternal
  l@(Bin_DataSetInternal sl xl ll lr)
  r@(Bin_DataSetInternal sr xr rl rr)
  | sl > sr
  = let !(m :*: l') = maxViewSure_DataSetInternal xl ll lr
    in balanceR_DataSetInternal m l' r
  | GHC.Base.otherwise
  = let !(m :*: r') = minViewSure_DataSetInternal xr rl rr
    in balanceL_DataSetInternal m l r'
deleteFindMin_DataSetInternal ::
  Set_DataSetInternal a -> (a, Set_DataSetInternal a)
deleteFindMin_DataSetInternal t
  | GHC.Maybe.Just r <- minView_DataSetInternal t = r
  | GHC.Base.otherwise
  = (GHC.Err.error
       "Set.deleteFindMin: can not return the minimal element of an empty set", 
     Tip_DataSetInternal)
deleteFindMax_DataSetInternal ::
  Set_DataSetInternal a -> (a, Set_DataSetInternal a)
deleteFindMax_DataSetInternal t
  | GHC.Maybe.Just r <- maxView_DataSetInternal t = r
  | GHC.Base.otherwise
  = (GHC.Err.error
       "Set.deleteFindMax: can not return the maximal element of an empty set", 
     Tip_DataSetInternal)
minViewSure_DataSetInternal ::
  a
  -> Set_DataSetInternal a
     -> Set_DataSetInternal a
        -> StrictPair_UtilsContainersInternalStrictPair a (Set_DataSetInternal a)
minViewSure_DataSetInternal
  = go
  where
      go x Tip_DataSetInternal r = x :*: r
      go x (Bin_DataSetInternal _ xl ll lr) r
        = case go xl ll lr of {
            xm :*: l' -> xm :*: balanceR_DataSetInternal x l' r }
minView_DataSetInternal ::
  Set_DataSetInternal a -> GHC.Maybe.Maybe (a, Set_DataSetInternal a)
minView_DataSetInternal Tip_DataSetInternal = GHC.Maybe.Nothing
minView_DataSetInternal (Bin_DataSetInternal _ x l r)
  = GHC.Maybe.Just $! toPair_UtilsContainersInternalStrictPair
      $ minViewSure_DataSetInternal x l r
maxViewSure_DataSetInternal ::
  a
  -> Set_DataSetInternal a
     -> Set_DataSetInternal a
        -> StrictPair_UtilsContainersInternalStrictPair a (Set_DataSetInternal a)
maxViewSure_DataSetInternal
  = go
  where
      go x l Tip_DataSetInternal = x :*: l
      go x l (Bin_DataSetInternal _ xr rl rr)
        = case go xr rl rr of {
            xm :*: r' -> xm :*: balanceL_DataSetInternal x l r' }
maxView_DataSetInternal ::
  Set_DataSetInternal a -> GHC.Maybe.Maybe (a, Set_DataSetInternal a)
maxView_DataSetInternal Tip_DataSetInternal = GHC.Maybe.Nothing
maxView_DataSetInternal (Bin_DataSetInternal _ x l r)
  = GHC.Maybe.Just $! toPair_UtilsContainersInternalStrictPair
      $ maxViewSure_DataSetInternal x l r
delta_DataSetInternal, ratio_DataSetInternal :: GHC.Types.Int
delta_DataSetInternal = 3
ratio_DataSetInternal = 2
balanceL_DataSetInternal ::
  a
  -> Set_DataSetInternal a
     -> Set_DataSetInternal a -> Set_DataSetInternal a
balanceL_DataSetInternal x l r
  = case r of
      Tip_DataSetInternal
        -> case l of
             Tip_DataSetInternal
               -> Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
             (Bin_DataSetInternal _ _ Tip_DataSetInternal Tip_DataSetInternal)
               -> Bin_DataSetInternal 2 x l Tip_DataSetInternal
             (Bin_DataSetInternal _
                                  lx
                                  Tip_DataSetInternal
                                  (Bin_DataSetInternal _ lrx _ _))
               -> Bin_DataSetInternal
                    3
                    lrx
                    (Bin_DataSetInternal 1 lx Tip_DataSetInternal Tip_DataSetInternal)
                    (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal)
             (Bin_DataSetInternal _
                                  lx
                                  ll@(Bin_DataSetInternal _ _ _ _)
                                  Tip_DataSetInternal)
               -> Bin_DataSetInternal
                    3
                    lx
                    ll
                    (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal)
             (Bin_DataSetInternal ls
                                  lx
                                  ll@(Bin_DataSetInternal lls _ _ _)
                                  lr@(Bin_DataSetInternal lrs lrx lrl lrr))
               | lrs < ratio_DataSetInternal * lls
               -> Bin_DataSetInternal
                    (1 + ls)
                    lx
                    ll
                    (Bin_DataSetInternal (1 + lrs) x lr Tip_DataSetInternal)
               | GHC.Base.otherwise
               -> Bin_DataSetInternal
                    (1 + ls)
                    lrx
                    (Bin_DataSetInternal
                       (1 + lls + size_DataSetInternal lrl) lx ll lrl)
                    (Bin_DataSetInternal
                       (1 + size_DataSetInternal lrr) x lrr Tip_DataSetInternal)
      (Bin_DataSetInternal rs _ _ _)
        -> case l of
             Tip_DataSetInternal
               -> Bin_DataSetInternal (1 + rs) x Tip_DataSetInternal r
             (Bin_DataSetInternal ls lx ll lr)
               | ls > delta_DataSetInternal * rs
               -> case (ll, lr) of
                    (Bin_DataSetInternal lls _ _ _,
                     Bin_DataSetInternal lrs lrx lrl lrr)
                      | lrs < ratio_DataSetInternal * lls
                      -> Bin_DataSetInternal
                           (1 + ls + rs) lx ll (Bin_DataSetInternal (1 + rs + lrs) x lr r)
                      | GHC.Base.otherwise
                      -> Bin_DataSetInternal
                           (1 + ls + rs)
                           lrx
                           (Bin_DataSetInternal
                              (1 + lls + size_DataSetInternal lrl) lx ll lrl)
                           (Bin_DataSetInternal (1 + rs + size_DataSetInternal lrr) x lrr r)
                    (_, _) -> GHC.Err.error "Failure in Data.Map.balanceL"
               | GHC.Base.otherwise -> Bin_DataSetInternal (1 + ls + rs) x l r
{-# NOINLINE balanceL_DataSetInternal #-}
balanceR_DataSetInternal ::
  a
  -> Set_DataSetInternal a
     -> Set_DataSetInternal a -> Set_DataSetInternal a
balanceR_DataSetInternal x l r
  = case l of
      Tip_DataSetInternal
        -> case r of
             Tip_DataSetInternal
               -> Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal
             (Bin_DataSetInternal _ _ Tip_DataSetInternal Tip_DataSetInternal)
               -> Bin_DataSetInternal 2 x Tip_DataSetInternal r
             (Bin_DataSetInternal _
                                  rx
                                  Tip_DataSetInternal
                                  rr@(Bin_DataSetInternal _ _ _ _))
               -> Bin_DataSetInternal
                    3
                    rx
                    (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal)
                    rr
             (Bin_DataSetInternal _
                                  rx
                                  (Bin_DataSetInternal _ rlx _ _)
                                  Tip_DataSetInternal)
               -> Bin_DataSetInternal
                    3
                    rlx
                    (Bin_DataSetInternal 1 x Tip_DataSetInternal Tip_DataSetInternal)
                    (Bin_DataSetInternal 1 rx Tip_DataSetInternal Tip_DataSetInternal)
             (Bin_DataSetInternal rs
                                  rx
                                  rl@(Bin_DataSetInternal rls rlx rll rlr)
                                  rr@(Bin_DataSetInternal rrs _ _ _))
               | rls < ratio_DataSetInternal * rrs
               -> Bin_DataSetInternal
                    (1 + rs)
                    rx
                    (Bin_DataSetInternal (1 + rls) x Tip_DataSetInternal rl)
                    rr
               | GHC.Base.otherwise
               -> Bin_DataSetInternal
                    (1 + rs)
                    rlx
                    (Bin_DataSetInternal
                       (1 + size_DataSetInternal rll) x Tip_DataSetInternal rll)
                    (Bin_DataSetInternal
                       (1 + rrs + size_DataSetInternal rlr) rx rlr rr)
      (Bin_DataSetInternal ls _ _ _)
        -> case r of
             Tip_DataSetInternal
               -> Bin_DataSetInternal (1 + ls) x l Tip_DataSetInternal
             (Bin_DataSetInternal rs rx rl rr)
               | rs > delta_DataSetInternal * ls
               -> case (rl, rr) of
                    (Bin_DataSetInternal rls rlx rll rlr,
                     Bin_DataSetInternal rrs _ _ _)
                      | rls < ratio_DataSetInternal * rrs
                      -> Bin_DataSetInternal
                           (1 + ls + rs) rx (Bin_DataSetInternal (1 + ls + rls) x l rl) rr
                      | GHC.Base.otherwise
                      -> Bin_DataSetInternal
                           (1 + ls + rs)
                           rlx
                           (Bin_DataSetInternal (1 + ls + size_DataSetInternal rll) x l rll)
                           (Bin_DataSetInternal
                              (1 + rrs + size_DataSetInternal rlr) rx rlr rr)
                    (_, _) -> GHC.Err.error "Failure in Data.Map.balanceR"
               | GHC.Base.otherwise -> Bin_DataSetInternal (1 + ls + rs) x l r
{-# NOINLINE balanceR_DataSetInternal #-}
bin_DataSetInternal ::
  a
  -> Set_DataSetInternal a
     -> Set_DataSetInternal a -> Set_DataSetInternal a
bin_DataSetInternal x l r
  = Bin_DataSetInternal
      (size_DataSetInternal l + size_DataSetInternal r + 1) x l r
{-# INLINE bin_DataSetInternal #-}
splitRoot_DataSetInternal ::
  Set_DataSetInternal a -> [Set_DataSetInternal a]
splitRoot_DataSetInternal orig
  = case orig of
      Tip_DataSetInternal -> []
      Bin_DataSetInternal _ v l r -> [l, singleton_DataSetInternal v, r]
{-# INLINE splitRoot_DataSetInternal #-}
powerSet_DataSetInternal ::
  Set_DataSetInternal a
  -> Set_DataSetInternal (Set_DataSetInternal a)
powerSet_DataSetInternal xs0
  = insertMin_DataSetInternal
      empty_DataSetInternal
      (foldr'_DataSetInternal step Tip_DataSetInternal xs0)
  where
      step x pxs
        = insertMin_DataSetInternal
            (singleton_DataSetInternal x)
            (insertMin_DataSetInternal x `mapMonotonic_DataSetInternal` pxs)
            `glue_DataSetInternal` pxs
cartesianProduct_DataSetInternal ::
  Set_DataSetInternal a
  -> Set_DataSetInternal b -> Set_DataSetInternal (a, b)
cartesianProduct_DataSetInternal as bs
  = getMergeSet_DataSetInternal
      $ Data.Foldable.foldMap
          (\ a
             -> MergeSet_DataSetInternal
                  $ mapMonotonic_DataSetInternal ((,) a) bs)
          as
newtype MergeSet_DataSetInternal a
  = MergeSet_DataSetInternal {getMergeSet_DataSetInternal :: Set_DataSetInternal a}
instance GHC.Base.Semigroup (MergeSet_DataSetInternal a) where
  MergeSet_DataSetInternal xs <> MergeSet_DataSetInternal ys
    = MergeSet_DataSetInternal (merge_DataSetInternal xs ys)
instance GHC.Base.Monoid (MergeSet_DataSetInternal a) where
  mempty = MergeSet_DataSetInternal empty_DataSetInternal
  mappend = (<>)
disjointUnion_DataSetInternal ::
  Set_DataSetInternal a
  -> Set_DataSetInternal b
     -> Set_DataSetInternal (Data.Either.Either a b)
disjointUnion_DataSetInternal as bs
  = merge_DataSetInternal
      (mapMonotonic_DataSetInternal Data.Either.Left as)
      (mapMonotonic_DataSetInternal Data.Either.Right bs)
showTree_DataSetInternal ::
  GHC.Show.Show a => Set_DataSetInternal a -> GHC.Base.String
showTree_DataSetInternal s
  = showTreeWith_DataSetInternal GHC.Types.True GHC.Types.False s
showTreeWith_DataSetInternal ::
  GHC.Show.Show a =>
  GHC.Types.Bool
  -> GHC.Types.Bool -> Set_DataSetInternal a -> GHC.Base.String
showTreeWith_DataSetInternal hang wide t
  | hang = (showsTreeHang_DataSetInternal wide [] t) ""
  | GHC.Base.otherwise = (showsTree_DataSetInternal wide [] [] t) ""
showsTree_DataSetInternal ::
  GHC.Show.Show a =>
  GHC.Types.Bool
  -> [GHC.Base.String]
     -> [GHC.Base.String] -> Set_DataSetInternal a -> GHC.Show.ShowS
showsTree_DataSetInternal wide lbars rbars t
  = case t of
      Tip_DataSetInternal
        -> showsBars_DataSetInternal lbars . GHC.Show.showString "|\n"
      Bin_DataSetInternal _ x Tip_DataSetInternal Tip_DataSetInternal
        -> showsBars_DataSetInternal lbars . GHC.Show.shows x
             . GHC.Show.showString "\n"
      Bin_DataSetInternal _ x l r
        -> showsTree_DataSetInternal
             wide
             (withBar_DataSetInternal rbars)
             (withEmpty_DataSetInternal rbars)
             r
             . showWide_DataSetInternal wide rbars
             . showsBars_DataSetInternal lbars
             . GHC.Show.shows x
             . GHC.Show.showString "\n"
             . showWide_DataSetInternal wide lbars
             . showsTree_DataSetInternal
                 wide
                 (withEmpty_DataSetInternal lbars)
                 (withBar_DataSetInternal lbars)
                 l
showsTreeHang_DataSetInternal ::
  GHC.Show.Show a =>
  GHC.Types.Bool
  -> [GHC.Base.String] -> Set_DataSetInternal a -> GHC.Show.ShowS
showsTreeHang_DataSetInternal wide bars t
  = case t of
      Tip_DataSetInternal
        -> showsBars_DataSetInternal bars . GHC.Show.showString "|\n"
      Bin_DataSetInternal _ x Tip_DataSetInternal Tip_DataSetInternal
        -> showsBars_DataSetInternal bars . GHC.Show.shows x
             . GHC.Show.showString "\n"
      Bin_DataSetInternal _ x l r
        -> showsBars_DataSetInternal bars . GHC.Show.shows x
             . GHC.Show.showString "\n"
             . showWide_DataSetInternal wide bars
             . showsTreeHang_DataSetInternal
                 wide (withBar_DataSetInternal bars) l
             . showWide_DataSetInternal wide bars
             . showsTreeHang_DataSetInternal
                 wide (withEmpty_DataSetInternal bars) r
showWide_DataSetInternal ::
  GHC.Types.Bool
  -> [GHC.Base.String] -> GHC.Base.String -> GHC.Base.String
showWide_DataSetInternal wide bars
  | wide
  = GHC.Show.showString
      (Data.Foldable.concat (GHC.List.reverse bars))
      . GHC.Show.showString "|\n"
  | GHC.Base.otherwise = GHC.Base.id
showsBars_DataSetInternal :: [GHC.Base.String] -> GHC.Show.ShowS
showsBars_DataSetInternal bars
  = case bars of
      [] -> GHC.Base.id
      _ -> GHC.Show.showString
             (Data.Foldable.concat (GHC.List.reverse (GHC.List.tail bars)))
             . GHC.Show.showString node_DataSetInternal
node_DataSetInternal :: GHC.Base.String
node_DataSetInternal = "+--"
withBar_DataSetInternal, withEmpty_DataSetInternal ::
  [GHC.Base.String] -> [GHC.Base.String]
withBar_DataSetInternal bars = "|  " : bars
withEmpty_DataSetInternal bars = "   " : bars
valid_DataSetInternal ::
  GHC.Classes.Ord a => Set_DataSetInternal a -> GHC.Types.Bool
valid_DataSetInternal t
  = balanced_DataSetInternal t && ordered_DataSetInternal t
      && validsize_DataSetInternal t
ordered_DataSetInternal ::
  GHC.Classes.Ord a => Set_DataSetInternal a -> GHC.Types.Bool
ordered_DataSetInternal t
  = bounded
      (GHC.Base.const GHC.Types.True) (GHC.Base.const GHC.Types.True) t
  where
      bounded lo hi t'
        = case t' of
            Tip_DataSetInternal -> GHC.Types.True
            Bin_DataSetInternal _ x l r
              -> (lo x) && (hi x) && bounded lo (< x) l && bounded (> x) hi r
balanced_DataSetInternal :: Set_DataSetInternal a -> GHC.Types.Bool
balanced_DataSetInternal t
  = case t of
      Tip_DataSetInternal -> GHC.Types.True
      Bin_DataSetInternal _ _ l r
        -> (size_DataSetInternal l + size_DataSetInternal r <= 1
              ||
                (size_DataSetInternal l <= delta_DataSetInternal
                   * size_DataSetInternal r
                   && size_DataSetInternal r
                   <= delta_DataSetInternal
                   * size_DataSetInternal l))
             && balanced_DataSetInternal l
             && balanced_DataSetInternal r
validsize_DataSetInternal ::
  Set_DataSetInternal a -> GHC.Types.Bool
validsize_DataSetInternal t
  = (realsize t == GHC.Maybe.Just (size_DataSetInternal t))
  where
      realsize t'
        = case t' of
            Tip_DataSetInternal -> GHC.Maybe.Just 0
            Bin_DataSetInternal sz _ l r
              -> case (realsize l, realsize r) of
                   (GHC.Maybe.Just n, GHC.Maybe.Just m)
                     | n + m + 1 == sz -> GHC.Maybe.Just sz
                   _ -> GHC.Maybe.Nothing
main
  = System.IO.print
      $ let
          {-# noinline f #-}
          f () = T2_Main
        in fromList_DataSetInternal [f (), f ()]
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
ptrEq_UtilsContainersInternalPtrEquality ::
  a -> a -> GHC.Types.Bool
hetPtrEq_UtilsContainersInternalPtrEquality ::
  a -> b -> GHC.Types.Bool
ptrEq_UtilsContainersInternalPtrEquality x y
  = GHC.Types.isTrue# (GHC.Prim.reallyUnsafePtrEquality# x y)
hetPtrEq_UtilsContainersInternalPtrEquality x y
  = GHC.Types.isTrue#
      (Unsafe.Coerce.unsafeCoerce GHC.Prim.reallyUnsafePtrEquality# x y)
{-# INLINE ptrEq_UtilsContainersInternalPtrEquality #-}
{-# INLINE hetPtrEq_UtilsContainersInternalPtrEquality #-}
infix 4 `ptrEq_UtilsContainersInternalPtrEquality`
infix 4 `hetPtrEq_UtilsContainersInternalPtrEquality`
data StrictPair_UtilsContainersInternalStrictPair a b = !a :*: !b
infixr 1 :*:
toPair_UtilsContainersInternalStrictPair ::
  StrictPair_UtilsContainersInternalStrictPair a b -> (a, b)
toPair_UtilsContainersInternalStrictPair (x :*: y) = (x, y)
{-# INLINE toPair_UtilsContainersInternalStrictPair #-}
