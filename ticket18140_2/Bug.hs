{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}

module AllInOne where
import qualified Data.Aeson
import qualified Data.Bits
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.UTF8
import qualified Data.Data
import qualified Data.Either
import qualified Data.Foldable
import qualified Data.Generics
import qualified Data.Int
import qualified Data.Ix
import qualified Data.Maybe
import qualified Data.Semigroup
import qualified Data.Sequence
import qualified Data.String
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import qualified Data.Typeable
import qualified Data.Word
import qualified GHC.Arr
import qualified GHC.Base
import qualified GHC.Classes
import qualified GHC.Enum
import qualified GHC.Err
import qualified GHC.Generics
import qualified GHC.Int
import qualified GHC.Maybe
import qualified GHC.Num
import qualified GHC.Read
import qualified GHC.Show
import qualified GHC.Types
import qualified GHC.Word
import qualified Text.ParserCombinators.ReadP
data FileOptions_TextDescriptorProtosFileOptions_AllInOne
  = FileOptions_TextDescriptorProtosFileOptions_AllInOne {java_package_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe Utf8_TextProtocolBuffersBasic),
                                                          java_outer_classname_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe Utf8_TextProtocolBuffersBasic),
                                                          java_multiple_files_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          java_generate_equals_and_hash_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          java_string_check_utf8_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          optimize_for_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne),
                                                          go_package_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe Utf8_TextProtocolBuffersBasic),
                                                          cc_generic_services_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          java_generic_services_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          py_generic_services_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          deprecated_TextDescriptorProtosFileOptions_AllInOne :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                          cc_enable_arenas_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          objc_class_prefix_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          csharp_namespace_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          javanano_use_deprecated_package_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          uninterpreted_option_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          ext'field_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          unknown'field_TextDescriptorProtosFileOptions_AllInOne :: ()}
  deriving (GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Mergeable_TextProtocolBuffersBasic FileOptions_TextDescriptorProtosFileOptions_AllInOne where
  mergeAppend_TextProtocolBuffersBasic
    (FileOptions_TextDescriptorProtosFileOptions_AllInOne x'1
                                                          x'2
                                                          x'3
                                                          x'4
                                                          x'5
                                                          x'6
                                                          x'7
                                                          x'8
                                                          x'9
                                                          x'10
                                                          x'11
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _)
    (FileOptions_TextDescriptorProtosFileOptions_AllInOne y'1
                                                          y'2
                                                          y'3
                                                          y'4
                                                          y'5
                                                          y'6
                                                          y'7
                                                          y'8
                                                          y'9
                                                          y'10
                                                          y'11
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _
                                                          _)
    = FileOptions_TextDescriptorProtosFileOptions_AllInOne
        (mergeAppend_TextProtocolBuffersBasic x'1 y'1)
        (mergeAppend_TextProtocolBuffersBasic x'2 y'2)
        (mergeAppend_TextProtocolBuffersBasic x'3 y'3)
        (mergeAppend_TextProtocolBuffersBasic x'4 y'4)
        (mergeAppend_TextProtocolBuffersBasic x'5 y'5)
        (mergeAppend_TextProtocolBuffersBasic x'6 y'6)
        (mergeAppend_TextProtocolBuffersBasic x'7 y'7)
        (mergeAppend_TextProtocolBuffersBasic x'8 y'8)
        (mergeAppend_TextProtocolBuffersBasic x'9 y'9)
        (mergeAppend_TextProtocolBuffersBasic x'10 y'10)
        (mergeAppend_TextProtocolBuffersBasic x'11 y'11)
        GHC.Err.undefined
        GHC.Err.undefined
        GHC.Err.undefined
        GHC.Err.undefined
        GHC.Err.undefined
        GHC.Err.undefined
        GHC.Err.undefined
instance Default_TextProtocolBuffersBasic FileOptions_TextDescriptorProtosFileOptions_AllInOne
data OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
  = LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
  deriving (GHC.Read.Read,
            GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Mergeable_TextProtocolBuffersBasic OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
instance Default_TextProtocolBuffersBasic OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
newtype Utf8_TextProtocolBuffersBasic
  = Utf8_TextProtocolBuffersBasic Data.ByteString.Lazy.ByteString
  deriving (Data.Data.Data,
            Data.Typeable.Typeable,
            GHC.Classes.Eq,
            GHC.Classes.Ord)
utf8_TextProtocolBuffersBasic ::
  Utf8_TextProtocolBuffersBasic -> Data.ByteString.Lazy.ByteString
utf8_TextProtocolBuffersBasic (Utf8_TextProtocolBuffersBasic bs)
  = bs
instance GHC.Read.Read Utf8_TextProtocolBuffersBasic where
  readsPrec d xs
    = let
        r ::
          GHC.Types.Int -> Text.ParserCombinators.ReadP.ReadS GHC.Base.String
        r = GHC.Read.readsPrec
        f ::
          (GHC.Base.String, GHC.Base.String)
          -> (Utf8_TextProtocolBuffersBasic, GHC.Base.String)
        f (a, b)
          = (Utf8_TextProtocolBuffersBasic
               (Data.ByteString.Lazy.UTF8.fromString a), 
             b)
      in GHC.Base.map f . r d $ xs
instance GHC.Show.Show Utf8_TextProtocolBuffersBasic where
  showsPrec d (Utf8_TextProtocolBuffersBasic bs)
    = let
        s :: GHC.Types.Int -> GHC.Base.String -> GHC.Show.ShowS
        s = GHC.Show.showsPrec
      in s d (Data.ByteString.Lazy.UTF8.toString bs)
instance Data.String.IsString Utf8_TextProtocolBuffersBasic where
  fromString = uFromString_TextProtocolBuffersBasic
instance GHC.Base.Semigroup Utf8_TextProtocolBuffersBasic where
  (<>)
    (Utf8_TextProtocolBuffersBasic x)
    (Utf8_TextProtocolBuffersBasic y)
    = Utf8_TextProtocolBuffersBasic (x <> y)
instance GHC.Base.Monoid Utf8_TextProtocolBuffersBasic where
  mempty = Utf8_TextProtocolBuffersBasic GHC.Base.mempty
  mappend = (<>)
instance Data.Aeson.ToJSON Utf8_TextProtocolBuffersBasic where
  toJSON (Utf8_TextProtocolBuffersBasic t)
    = Data.Aeson.toJSON (Data.Text.Lazy.Encoding.decodeUtf8 t)
instance Data.Aeson.FromJSON Utf8_TextProtocolBuffersBasic where
  parseJSON value
    = case value of
        Data.Aeson.String t
          -> GHC.Base.return . Utf8_TextProtocolBuffersBasic
               . Data.Text.Lazy.Encoding.encodeUtf8
               . Data.Text.Lazy.fromStrict
               $ t
        _ -> fail
               ("Value " ++ GHC.Show.show value ++ " is not a UTF-8 string")
newtype WireTag_TextProtocolBuffersBasic
  = WireTag_TextProtocolBuffersBasic {getWireTag_TextProtocolBuffersBasic :: GHC.Word.Word32}
  deriving (GHC.Classes.Eq,
            GHC.Classes.Ord,
            GHC.Enum.Enum,
            GHC.Read.Read,
            GHC.Show.Show,
            GHC.Num.Num,
            Data.Bits.Bits,
            GHC.Enum.Bounded,
            Data.Data.Data,
            Data.Typeable.Typeable)
newtype FieldId_TextProtocolBuffersBasic
  = FieldId_TextProtocolBuffersBasic {getFieldId_TextProtocolBuffersBasic :: GHC.Int.Int32}
  deriving (GHC.Classes.Eq,
            GHC.Classes.Ord,
            GHC.Enum.Enum,
            GHC.Read.Read,
            GHC.Show.Show,
            GHC.Num.Num,
            Data.Data.Data,
            Data.Typeable.Typeable,
            GHC.Arr.Ix)
instance GHC.Enum.Bounded FieldId_TextProtocolBuffersBasic where
  minBound = 1
  maxBound = 536870911
newtype WireType_TextProtocolBuffersBasic
  = WireType_TextProtocolBuffersBasic {getWireType_TextProtocolBuffersBasic :: GHC.Word.Word32}
  deriving (GHC.Classes.Eq,
            GHC.Classes.Ord,
            GHC.Enum.Enum,
            GHC.Read.Read,
            GHC.Show.Show,
            GHC.Num.Num,
            Data.Data.Data,
            Data.Typeable.Typeable)
instance GHC.Enum.Bounded WireType_TextProtocolBuffersBasic where
  minBound = 0
  maxBound = 5
newtype FieldType_TextProtocolBuffersBasic
  = FieldType_TextProtocolBuffersBasic {getFieldType_TextProtocolBuffersBasic :: GHC.Types.Int}
  deriving (GHC.Classes.Eq,
            GHC.Classes.Ord,
            GHC.Enum.Enum,
            GHC.Read.Read,
            GHC.Show.Show,
            GHC.Num.Num,
            Data.Data.Data,
            Data.Typeable.Typeable)
instance GHC.Enum.Bounded FieldType_TextProtocolBuffersBasic where
  minBound = 1
  maxBound = 18
newtype EnumCode_TextProtocolBuffersBasic
  = EnumCode_TextProtocolBuffersBasic {getEnumCode_TextProtocolBuffersBasic :: GHC.Int.Int32}
  deriving (GHC.Classes.Eq,
            GHC.Classes.Ord,
            GHC.Read.Read,
            GHC.Show.Show,
            GHC.Num.Num,
            Data.Data.Data,
            Data.Typeable.Typeable)
instance GHC.Enum.Bounded EnumCode_TextProtocolBuffersBasic where
  minBound = 0
  maxBound = 2147483647
type WireSize_TextProtocolBuffersBasic = GHC.Int.Int64
class Default_TextProtocolBuffersBasic a => Mergeable_TextProtocolBuffersBasic a where
  mergeAppend_TextProtocolBuffersBasic :: a -> a -> a
  mergeAppend_TextProtocolBuffersBasic _a b = b
  mergeConcat_TextProtocolBuffersBasic ::
    Data.Foldable.Foldable t => t a -> a
  mergeConcat_TextProtocolBuffersBasic
    = Data.Foldable.foldl
        mergeAppend_TextProtocolBuffersBasic
        defaultValue_TextProtocolBuffersBasic
class Default_TextProtocolBuffersBasic a where
  defaultValue_TextProtocolBuffersBasic :: a
isValidUTF8_TextProtocolBuffersBasic ::
  Data.ByteString.Lazy.ByteString -> GHC.Maybe.Maybe GHC.Types.Int
isValidUTF8_TextProtocolBuffersBasic bs
  = go 0 (Data.ByteString.Lazy.unpack bs) 0
  where
      go ::
        GHC.Types.Int
        -> [GHC.Word.Word8]
           -> GHC.Types.Int -> GHC.Maybe.Maybe GHC.Types.Int
      go 0 [] _ = GHC.Maybe.Nothing
      go 0 (x : xs) n
        | x <= 127 = go 0 xs $! GHC.Enum.succ n
        | x <= 193 = GHC.Maybe.Just n
        | x <= 223 = go 1 xs $! GHC.Enum.succ n
        | x <= 239 = go 2 xs $! GHC.Enum.succ n
        | x <= 243 = go 3 xs $! GHC.Enum.succ n
        | x == 244 = high xs $! GHC.Enum.succ n
        | GHC.Base.otherwise = GHC.Maybe.Just n
      go i (x : xs) n
        | 128 <= x && x <= 191 = go (GHC.Enum.pred i) xs $! GHC.Enum.succ n
      go _ _ n = GHC.Maybe.Just n
      high (x : xs) n
        | 128 <= x && x <= 143 = go 2 xs $! GHC.Enum.succ n
        | GHC.Base.otherwise = GHC.Maybe.Just n
      high [] n = GHC.Maybe.Just n
toUtf8_TextProtocolBuffersBasic ::
  Data.ByteString.Lazy.ByteString
  -> Data.Either.Either GHC.Types.Int Utf8_TextProtocolBuffersBasic
toUtf8_TextProtocolBuffersBasic bs
  = Data.Maybe.maybe
      (Data.Either.Right (Utf8_TextProtocolBuffersBasic bs))
      Data.Either.Left
      (isValidUTF8_TextProtocolBuffersBasic bs)
uToString_TextProtocolBuffersBasic ::
  Utf8_TextProtocolBuffersBasic -> GHC.Base.String
uToString_TextProtocolBuffersBasic
  (Utf8_TextProtocolBuffersBasic bs)
  = Data.ByteString.Lazy.UTF8.toString bs
uFromString_TextProtocolBuffersBasic ::
  GHC.Base.String -> Utf8_TextProtocolBuffersBasic
uFromString_TextProtocolBuffersBasic s
  = Utf8_TextProtocolBuffersBasic
      (Data.ByteString.Lazy.UTF8.fromString s)
instance Mergeable_TextProtocolBuffersBasic a =>
         Mergeable_TextProtocolBuffersBasic (GHC.Maybe.Maybe a) where
  mergeAppend_TextProtocolBuffersBasic
    = mayMerge_TextProtocolBuffersBasic
{-# INLINE mayMerge_TextProtocolBuffersBasic #-}
mayMerge_TextProtocolBuffersBasic ::
  (Mergeable_TextProtocolBuffersBasic b) =>
  GHC.Maybe.Maybe b -> GHC.Maybe.Maybe b -> GHC.Maybe.Maybe b
mayMerge_TextProtocolBuffersBasic GHC.Maybe.Nothing y = y
mayMerge_TextProtocolBuffersBasic x GHC.Maybe.Nothing = x
mayMerge_TextProtocolBuffersBasic
  (GHC.Maybe.Just x)
  (GHC.Maybe.Just y)
  = GHC.Maybe.Just (mergeAppend_TextProtocolBuffersBasic x y)
instance Mergeable_TextProtocolBuffersBasic (Data.Sequence.Seq a) where
  mergeAppend_TextProtocolBuffersBasic = (Data.Sequence.><)
instance Mergeable_TextProtocolBuffersBasic GHC.Types.Bool
instance Mergeable_TextProtocolBuffersBasic Utf8_TextProtocolBuffersBasic
instance Mergeable_TextProtocolBuffersBasic Data.ByteString.Lazy.ByteString
instance Mergeable_TextProtocolBuffersBasic GHC.Types.Double
instance Mergeable_TextProtocolBuffersBasic GHC.Types.Float
instance Mergeable_TextProtocolBuffersBasic GHC.Int.Int32
instance Mergeable_TextProtocolBuffersBasic GHC.Int.Int64
instance Mergeable_TextProtocolBuffersBasic GHC.Word.Word32
instance Mergeable_TextProtocolBuffersBasic GHC.Word.Word64
instance Default_TextProtocolBuffersBasic GHC.Word.Word64 where
  defaultValue_TextProtocolBuffersBasic = 0
instance Default_TextProtocolBuffersBasic GHC.Word.Word32 where
  defaultValue_TextProtocolBuffersBasic = 0
instance Default_TextProtocolBuffersBasic GHC.Int.Int64 where
  defaultValue_TextProtocolBuffersBasic = 0
instance Default_TextProtocolBuffersBasic GHC.Int.Int32 where
  defaultValue_TextProtocolBuffersBasic = 0
instance Default_TextProtocolBuffersBasic GHC.Types.Float where
  defaultValue_TextProtocolBuffersBasic = 0
instance Default_TextProtocolBuffersBasic GHC.Types.Double where
  defaultValue_TextProtocolBuffersBasic = 0
instance Default_TextProtocolBuffersBasic GHC.Types.Bool where
  defaultValue_TextProtocolBuffersBasic = GHC.Types.False
instance Default_TextProtocolBuffersBasic (GHC.Maybe.Maybe a) where
  defaultValue_TextProtocolBuffersBasic = GHC.Maybe.Nothing
instance Default_TextProtocolBuffersBasic (Data.Sequence.Seq a) where
  defaultValue_TextProtocolBuffersBasic = GHC.Base.mempty
instance Default_TextProtocolBuffersBasic Data.ByteString.Lazy.ByteString where
  defaultValue_TextProtocolBuffersBasic = GHC.Base.mempty
instance Default_TextProtocolBuffersBasic Utf8_TextProtocolBuffersBasic where
  defaultValue_TextProtocolBuffersBasic = GHC.Base.mempty
