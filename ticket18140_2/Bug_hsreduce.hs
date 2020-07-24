{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, GeneralizedNewtypeDeriving, CPP #-}
module AllInOne (
    ) where
import Data.Aeson
import Data.ByteString.Lazy
import Data.Sequence
import Data.String
import GHC.Base
import GHC.Enum
import GHC.Int
import GHC.Maybe
import GHC.Read
import GHC.Show
import GHC.Types
import GHC.Word
data FileOptions_TextDescriptorProtosFileOptions_AllInOne
  = FileOptions_TextDescriptorProtosFileOptions_AllInOne () () () !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) () () () () () () ()
instance Mergeable_TextProtocolBuffersBasic FileOptions_TextDescriptorProtosFileOptions_AllInOne where
  mergeAppend_TextProtocolBuffersBasic
    (FileOptions_TextDescriptorProtosFileOptions_AllInOne _
                                                          _
                                                          _
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
    (FileOptions_TextDescriptorProtosFileOptions_AllInOne _
                                                          _
                                                          _
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
        undefined
        undefined
        undefined
        (mergeAppend_TextProtocolBuffersBasic x'4 y'4)
        (mergeAppend_TextProtocolBuffersBasic x'5 y'5)
        (mergeAppend_TextProtocolBuffersBasic x'6 y'6)
        (mergeAppend_TextProtocolBuffersBasic x'7 y'7)
        (mergeAppend_TextProtocolBuffersBasic x'8 y'8)
        (mergeAppend_TextProtocolBuffersBasic x'9 y'9)
        (mergeAppend_TextProtocolBuffersBasic x'10 y'10)
        (mergeAppend_TextProtocolBuffersBasic x'11 y'11)
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
instance Default_TextProtocolBuffersBasic FileOptions_TextDescriptorProtosFileOptions_AllInOne
data OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
  = LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
instance Mergeable_TextProtocolBuffersBasic OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
instance Default_TextProtocolBuffersBasic OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode_AllInOne
newtype Utf8_TextProtocolBuffersBasic
  = Utf8_TextProtocolBuffersBasic ()
instance Read Utf8_TextProtocolBuffersBasic
instance Show Utf8_TextProtocolBuffersBasic
instance IsString ()
instance Semigroup Utf8_TextProtocolBuffersBasic
instance Monoid Utf8_TextProtocolBuffersBasic
instance ToJSON Utf8_TextProtocolBuffersBasic where
  toJSON _ = undefined
instance FromJSON Utf8_TextProtocolBuffersBasic where
  parseJSON _ = undefined
newtype WireTag_TextProtocolBuffersBasic
  = WireTag_TextProtocolBuffersBasic Word32
newtype FieldId_TextProtocolBuffersBasic
  = FieldId_TextProtocolBuffersBasic Int32
instance Bounded FieldId_TextProtocolBuffersBasic
newtype WireType_TextProtocolBuffersBasic
  = WireType_TextProtocolBuffersBasic Word32
instance Bounded WireType_TextProtocolBuffersBasic
newtype FieldType_TextProtocolBuffersBasic
  = FieldType_TextProtocolBuffersBasic Int
instance Bounded FieldType_TextProtocolBuffersBasic
newtype EnumCode_TextProtocolBuffersBasic
  = EnumCode_TextProtocolBuffersBasic Int32
instance Bounded EnumCode_TextProtocolBuffersBasic
class Mergeable_TextProtocolBuffersBasic a where
  mergeAppend_TextProtocolBuffersBasic :: a -> a -> a
  mergeConcat_TextProtocolBuffersBasic :: () -> a
class Default_TextProtocolBuffersBasic a where
  defaultValue_TextProtocolBuffersBasic :: a
instance Mergeable_TextProtocolBuffersBasic (Maybe a) where
  mergeAppend_TextProtocolBuffersBasic
    = mayMerge_TextProtocolBuffersBasic
{-# INLINE mayMerge_TextProtocolBuffersBasic #-}
mayMerge_TextProtocolBuffersBasic :: Maybe b -> Maybe b -> Maybe b
mayMerge_TextProtocolBuffersBasic Nothing y = y
mayMerge_TextProtocolBuffersBasic x Nothing = x
mayMerge_TextProtocolBuffersBasic _ _ = Just undefined
instance Mergeable_TextProtocolBuffersBasic ()
instance Mergeable_TextProtocolBuffersBasic Bool
instance Mergeable_TextProtocolBuffersBasic Utf8_TextProtocolBuffersBasic
instance Mergeable_TextProtocolBuffersBasic ByteString
instance Mergeable_TextProtocolBuffersBasic Double
instance Mergeable_TextProtocolBuffersBasic Float
instance Mergeable_TextProtocolBuffersBasic Int32
instance Mergeable_TextProtocolBuffersBasic Int64
instance Mergeable_TextProtocolBuffersBasic Word32
instance Mergeable_TextProtocolBuffersBasic Word64
instance Default_TextProtocolBuffersBasic ()
instance Default_TextProtocolBuffersBasic Word32
instance Default_TextProtocolBuffersBasic Int64
instance Default_TextProtocolBuffersBasic Int32
instance Default_TextProtocolBuffersBasic Float
instance Default_TextProtocolBuffersBasic Double
instance Default_TextProtocolBuffersBasic Bool
instance Default_TextProtocolBuffersBasic (Maybe a)
instance Default_TextProtocolBuffersBasic (Seq a)
instance Default_TextProtocolBuffersBasic ByteString
instance Default_TextProtocolBuffersBasic Utf8_TextProtocolBuffersBasic
