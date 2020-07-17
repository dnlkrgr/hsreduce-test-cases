{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module AllInOne () where

import qualified Data.Binary.Put
import qualified Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Lazy.Internal
import qualified Data.Data
import qualified Data.Foldable
import qualified Data.Maybe
import qualified Data.Sequence.Internal
import qualified Data.Set.Internal
import qualified Data.Tuple
import qualified Data.Typeable
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
import qualified Text.Parsec.Char
import qualified Text.Parsec.Combinator
import qualified Text.Parsec.Prim
import qualified Text.ProtocolBuffers.Basic
import qualified Text.ProtocolBuffers.Extensions
import qualified Text.ProtocolBuffers.Header
import qualified Text.ProtocolBuffers.Reflections
import qualified Text.ProtocolBuffers.TextMessage
import qualified Text.ProtocolBuffers.Unknown
import qualified Text.ProtocolBuffers.WireMessage
import qualified Text.Read

data FileOptions_TextDescriptorProtosFileOptions
  = FileOptions_TextDescriptorProtosFileOptions {java_package_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                 java_outer_classname_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                 java_multiple_files_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 java_generate_equals_and_hash_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 java_string_check_utf8_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 optimize_for_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode),
                                                 go_package_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                 cc_generic_services_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 java_generic_services_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 py_generic_services_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 deprecated_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 cc_enable_arenas_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 objc_class_prefix_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                 csharp_namespace_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                 javanano_use_deprecated_package_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 uninterpreted_option_TextDescriptorProtosFileOptions :: !(Data.Sequence.Internal.Seq UninterpretedOption_TextDescriptorProtosUninterpretedOption),
                                                 ext'field_TextDescriptorProtosFileOptions :: !(Text.ProtocolBuffers.Extensions.ExtField),
                                                 unknown'field_TextDescriptorProtosFileOptions :: !(Text.ProtocolBuffers.Unknown.UnknownField)}
  deriving (GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Text.ProtocolBuffers.Extensions.ExtendMessage FileOptions_TextDescriptorProtosFileOptions where
  getExtField = ext'field_TextDescriptorProtosFileOptions
  putExtField e'f msg
    = msg {ext'field_TextDescriptorProtosFileOptions = e'f}
  validExtRanges msg
    = Text.ProtocolBuffers.Reflections.extRanges
        (Text.ProtocolBuffers.Reflections.reflectDescriptorInfo msg)
instance Text.ProtocolBuffers.Unknown.UnknownMessage FileOptions_TextDescriptorProtosFileOptions where
  getUnknownField = unknown'field_TextDescriptorProtosFileOptions
  putUnknownField u'f msg
    = msg {unknown'field_TextDescriptorProtosFileOptions = u'f}
instance Text.ProtocolBuffers.Basic.Mergeable FileOptions_TextDescriptorProtosFileOptions where
  mergeAppend
    (FileOptions_TextDescriptorProtosFileOptions x'1
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
                                                 x'12
                                                 x'13
                                                 x'14
                                                 x'15
                                                 x'16
                                                 x'17
                                                 x'18)
    (FileOptions_TextDescriptorProtosFileOptions y'1
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
                                                 y'12
                                                 y'13
                                                 y'14
                                                 y'15
                                                 y'16
                                                 y'17
                                                 y'18)
    = FileOptions_TextDescriptorProtosFileOptions
        (Text.ProtocolBuffers.Basic.mergeAppend x'1 y'1)
        (Text.ProtocolBuffers.Basic.mergeAppend x'2 y'2)
        (Text.ProtocolBuffers.Basic.mergeAppend x'3 y'3)
        (Text.ProtocolBuffers.Basic.mergeAppend x'4 y'4)
        (Text.ProtocolBuffers.Basic.mergeAppend x'5 y'5)
        (Text.ProtocolBuffers.Basic.mergeAppend x'6 y'6)
        (Text.ProtocolBuffers.Basic.mergeAppend x'7 y'7)
        (Text.ProtocolBuffers.Basic.mergeAppend x'8 y'8)
        (Text.ProtocolBuffers.Basic.mergeAppend x'9 y'9)
        (Text.ProtocolBuffers.Basic.mergeAppend x'10 y'10)
        (Text.ProtocolBuffers.Basic.mergeAppend x'11 y'11)
        (Text.ProtocolBuffers.Basic.mergeAppend x'12 y'12)
        (Text.ProtocolBuffers.Basic.mergeAppend x'13 y'13)
        (Text.ProtocolBuffers.Basic.mergeAppend x'14 y'14)
        (Text.ProtocolBuffers.Basic.mergeAppend x'15 y'15)
        (Text.ProtocolBuffers.Basic.mergeAppend x'16 y'16)
        (Text.ProtocolBuffers.Basic.mergeAppend x'17 y'17)
        (Text.ProtocolBuffers.Basic.mergeAppend x'18 y'18)
instance Text.ProtocolBuffers.Basic.Default FileOptions_TextDescriptorProtosFileOptions where
  defaultValue
    = FileOptions_TextDescriptorProtosFileOptions
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just (Text.Read.read "SPEED"))
        Text.ProtocolBuffers.Basic.defaultValue
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just GHC.Types.False)
        (GHC.Maybe.Just GHC.Types.False)
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
instance Text.ProtocolBuffers.WireMessage.Wire FileOptions_TextDescriptorProtosFileOptions where
  wireSize
    ft'
    self'@(FileOptions_TextDescriptorProtosFileOptions x'1
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
                                                       x'12
                                                       x'13
                                                       x'14
                                                       x'15
                                                       x'16
                                                       x'17
                                                       x'18)
    = case ft' of
        10 -> calc'Size
        11 -> Text.ProtocolBuffers.WireMessage.prependMessageSize calc'Size
        _ -> Text.ProtocolBuffers.WireMessage.wireSizeErr ft' self'
    where
        calc'Size
          = (Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 9 x'1
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 9 x'2
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 8 x'3
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'4
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'5
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 14 x'6
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 9 x'7
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'8
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'9
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'10
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'11
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'12
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 9 x'13
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 9 x'14
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 2 8 x'15
               + Text.ProtocolBuffers.WireMessage.wireSizeRep 2 11 x'16
               + Text.ProtocolBuffers.Extensions.wireSizeExtField x'17
               + Text.ProtocolBuffers.Unknown.wireSizeUnknownField x'18)
  wirePutWithSize
    ft'
    self'@(FileOptions_TextDescriptorProtosFileOptions x'1
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
                                                       x'12
                                                       x'13
                                                       x'14
                                                       x'15
                                                       x'16
                                                       x'17
                                                       x'18)
    = case ft' of
        10 -> put'Fields
        11 -> put'FieldsSized
        _ -> Text.ProtocolBuffers.WireMessage.wirePutErr ft' self'
    where
        put'Fields
          = Text.ProtocolBuffers.WireMessage.sequencePutWithSize
              [Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 10 9 x'1,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 66 9 x'2,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 72 14 x'6,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 80 8 x'3,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 90 9 x'7,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 128 8 x'8,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 136 8 x'9,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 144 8 x'10,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 160 8 x'4,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 184 8 x'11,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 216 8 x'5,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 248 8 x'12,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 290 9 x'13,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 298 9 x'14,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 304 8 x'15,
               Text.ProtocolBuffers.WireMessage.wirePutRepWithSize 7994 11 x'16,
               Text.ProtocolBuffers.Extensions.wirePutExtFieldWithSize x'17,
               Text.ProtocolBuffers.Unknown.wirePutUnknownFieldWithSize x'18]
        put'FieldsSized
          = let
              size' = Data.Tuple.fst (Data.Binary.Put.runPutM put'Fields)
              put'Size
                = do Text.ProtocolBuffers.WireMessage.putSize size'
                     GHC.Base.return
                       (Text.ProtocolBuffers.WireMessage.size'WireSize size')
            in
              Text.ProtocolBuffers.WireMessage.sequencePutWithSize
                [put'Size, put'Fields]
  wireGet ft'
    = case ft' of
        10
          -> Text.ProtocolBuffers.WireMessage.getBareMessageWith
               (Text.ProtocolBuffers.Unknown.catch'Unknown update'Self)
        11
          -> Text.ProtocolBuffers.WireMessage.getMessageWith
               (Text.ProtocolBuffers.Unknown.catch'Unknown update'Self)
        _ -> Text.ProtocolBuffers.WireMessage.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
          = case wire'Tag of
              10
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {java_package_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                               new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              66
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {java_outer_classname_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                       new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              80
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {java_multiple_files_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                      new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              160
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {java_generate_equals_and_hash_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                                new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              216
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {java_string_check_utf8_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                         new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              72
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {optimize_for_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                               new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 14)
              90
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {go_package_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                             new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              128
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {cc_generic_services_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                      new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              136
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {java_generic_services_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                        new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              144
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {py_generic_services_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                      new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              184
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {deprecated_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                             new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              248
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {cc_enable_arenas_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                   new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              290
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {objc_class_prefix_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                    new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              298
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {csharp_namespace_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                   new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              304
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {javanano_use_deprecated_package_TextDescriptorProtosFileOptions = GHC.Maybe.Just
                                                                                                  new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              7994
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {uninterpreted_option_TextDescriptorProtosFileOptions = Text.ProtocolBuffers.Header.append
                                                                                       (uninterpreted_option_TextDescriptorProtosFileOptions
                                                                                          old'Self)
                                                                                       new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 11)
              _ -> let
                     (field'Number, wire'Type)
                       = Text.ProtocolBuffers.WireMessage.splitWireTag wire'Tag
                   in
                     if Data.Foldable.or
                          [1000 <= field'Number && field'Number <= 18999,
                           20000 <= field'Number] then
                         Text.ProtocolBuffers.Extensions.loadExtension
                           field'Number wire'Type old'Self
                     else
                         Text.ProtocolBuffers.WireMessage.unknown
                           field'Number wire'Type old'Self
instance Text.ProtocolBuffers.Extensions.MessageAPI msg' (msg'
                                                          -> FileOptions_TextDescriptorProtosFileOptions) FileOptions_TextDescriptorProtosFileOptions where
  getVal m' f' = f' m'
instance Text.ProtocolBuffers.Extensions.GPB FileOptions_TextDescriptorProtosFileOptions
instance Text.ProtocolBuffers.Reflections.ReflectDescriptor FileOptions_TextDescriptorProtosFileOptions where
  getMessageInfo _
    = Text.ProtocolBuffers.Reflections.GetMessageInfo
        (Data.Set.Internal.fromDistinctAscList [])
        (Data.Set.Internal.fromDistinctAscList
           [10, 66, 72, 80, 90, 128, 136, 144, 160, 184, 216, 248, 290, 298,
            304, 7994])
  reflectDescriptorInfo _
    = Text.Read.read
        "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.FileOptions\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"FileOptions\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"FileOptions.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_outer_classname\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_outer_classname\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_multiple_files\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_multiple_files\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 10}, wireTag = WireTag {getWireTag = 80}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_generate_equals_and_hash\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_generate_equals_and_hash\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 20}, wireTag = WireTag {getWireTag = 160}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_string_check_utf8\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_string_check_utf8\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 27}, wireTag = WireTag {getWireTag = 216}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.optimize_for\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"optimize_for\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 9}, wireTag = WireTag {getWireTag = 72}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.FileOptions.OptimizeMode\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName = MName \"OptimizeMode\"}), hsRawDefault = Just \"SPEED\", hsDefault = Just (HsDef'Enum \"SPEED\")},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.go_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"go_package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 11}, wireTag = WireTag {getWireTag = 90}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.cc_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"cc_generic_services\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 16}, wireTag = WireTag {getWireTag = 128}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.java_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"java_generic_services\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 17}, wireTag = WireTag {getWireTag = 136}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.py_generic_services\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"py_generic_services\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 18}, wireTag = WireTag {getWireTag = 144}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.deprecated\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"deprecated\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 23}, wireTag = WireTag {getWireTag = 184}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.cc_enable_arenas\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"cc_enable_arenas\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 31}, wireTag = WireTag {getWireTag = 248}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Just \"false\", hsDefault = Just (HsDef'Bool False)},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.objc_class_prefix\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"objc_class_prefix\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 36}, wireTag = WireTag {getWireTag = 290}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.csharp_namespace\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"csharp_namespace\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 37}, wireTag = WireTag {getWireTag = 298}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.javanano_use_deprecated_package\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"javanano_use_deprecated_package\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 38}, wireTag = WireTag {getWireTag = 304}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.FileOptions.uninterpreted_option\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"FileOptions\"], baseName' = FName \"uninterpreted_option\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 999}, wireTag = WireTag {getWireTag = 7994}, packedTag = Nothing, wireTagLength = 2, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}), hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [(FieldId {getFieldId = 1000},FieldId {getFieldId = 18999}),(FieldId {getFieldId = 20000},FieldId {getFieldId = 536870911})], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"
instance Text.ProtocolBuffers.TextMessage.TextType FileOptions_TextDescriptorProtosFileOptions where
  tellT = Text.ProtocolBuffers.TextMessage.tellSubMessage
  getT = Text.ProtocolBuffers.TextMessage.getSubMessage
instance Text.ProtocolBuffers.TextMessage.TextMsg FileOptions_TextDescriptorProtosFileOptions where
  textPut msg
    = do Text.ProtocolBuffers.TextMessage.tellT
           "java_package" (java_package_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "java_outer_classname"
           (java_outer_classname_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "java_multiple_files"
           (java_multiple_files_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "java_generate_equals_and_hash"
           (java_generate_equals_and_hash_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "java_string_check_utf8"
           (java_string_check_utf8_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "optimize_for" (optimize_for_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "go_package" (go_package_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "cc_generic_services"
           (cc_generic_services_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "java_generic_services"
           (java_generic_services_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "py_generic_services"
           (py_generic_services_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "deprecated" (deprecated_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "cc_enable_arenas"
           (cc_enable_arenas_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "objc_class_prefix"
           (objc_class_prefix_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "csharp_namespace"
           (csharp_namespace_TextDescriptorProtosFileOptions msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "javanano_use_deprecated_package"
           (javanano_use_deprecated_package_TextDescriptorProtosFileOptions
              msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "uninterpreted_option"
           (uninterpreted_option_TextDescriptorProtosFileOptions msg)
  textGet
    = do mods <- Text.Parsec.Combinator.sepEndBy
                   (Text.Parsec.Combinator.choice
                      [parse'java_package, parse'java_outer_classname,
                       parse'java_multiple_files, parse'java_generate_equals_and_hash,
                       parse'java_string_check_utf8, parse'optimize_for, parse'go_package,
                       parse'cc_generic_services, parse'java_generic_services,
                       parse'py_generic_services, parse'deprecated,
                       parse'cc_enable_arenas, parse'objc_class_prefix,
                       parse'csharp_namespace, parse'javanano_use_deprecated_package,
                       parse'uninterpreted_option])
                   Text.Parsec.Char.spaces
         GHC.Base.return
           (Data.Foldable.foldl
              (\ v f -> f v) Text.ProtocolBuffers.Basic.defaultValue mods)
    where
        parse'java_package
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "java_package"
                  GHC.Base.return
                    (\ o -> o {java_package_TextDescriptorProtosFileOptions = v}))
        parse'java_outer_classname
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "java_outer_classname"
                  GHC.Base.return
                    (\ o
                       -> o {java_outer_classname_TextDescriptorProtosFileOptions = v}))
        parse'java_multiple_files
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "java_multiple_files"
                  GHC.Base.return
                    (\ o
                       -> o {java_multiple_files_TextDescriptorProtosFileOptions = v}))
        parse'java_generate_equals_and_hash
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "java_generate_equals_and_hash"
                  GHC.Base.return
                    (\ o
                       -> o {java_generate_equals_and_hash_TextDescriptorProtosFileOptions = v}))
        parse'java_string_check_utf8
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "java_string_check_utf8"
                  GHC.Base.return
                    (\ o
                       -> o {java_string_check_utf8_TextDescriptorProtosFileOptions = v}))
        parse'optimize_for
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "optimize_for"
                  GHC.Base.return
                    (\ o -> o {optimize_for_TextDescriptorProtosFileOptions = v}))
        parse'go_package
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "go_package"
                  GHC.Base.return
                    (\ o -> o {go_package_TextDescriptorProtosFileOptions = v}))
        parse'cc_generic_services
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "cc_generic_services"
                  GHC.Base.return
                    (\ o
                       -> o {cc_generic_services_TextDescriptorProtosFileOptions = v}))
        parse'java_generic_services
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "java_generic_services"
                  GHC.Base.return
                    (\ o
                       -> o {java_generic_services_TextDescriptorProtosFileOptions = v}))
        parse'py_generic_services
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "py_generic_services"
                  GHC.Base.return
                    (\ o
                       -> o {py_generic_services_TextDescriptorProtosFileOptions = v}))
        parse'deprecated
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "deprecated"
                  GHC.Base.return
                    (\ o -> o {deprecated_TextDescriptorProtosFileOptions = v}))
        parse'cc_enable_arenas
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "cc_enable_arenas"
                  GHC.Base.return
                    (\ o -> o {cc_enable_arenas_TextDescriptorProtosFileOptions = v}))
        parse'objc_class_prefix
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "objc_class_prefix"
                  GHC.Base.return
                    (\ o -> o {objc_class_prefix_TextDescriptorProtosFileOptions = v}))
        parse'csharp_namespace
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "csharp_namespace"
                  GHC.Base.return
                    (\ o -> o {csharp_namespace_TextDescriptorProtosFileOptions = v}))
        parse'javanano_use_deprecated_package
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "javanano_use_deprecated_package"
                  GHC.Base.return
                    (\ o
                       -> o {javanano_use_deprecated_package_TextDescriptorProtosFileOptions = v}))
        parse'uninterpreted_option
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT
                         "uninterpreted_option"
                  GHC.Base.return
                    (\ o
                       -> o {uninterpreted_option_TextDescriptorProtosFileOptions = Text.ProtocolBuffers.Header.append
                                                                                      (uninterpreted_option_TextDescriptorProtosFileOptions
                                                                                         o)
                                                                                      v}))
data OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
  = SPEED_TextDescriptorProtosFileOptionsOptimizeMode |
    CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode |
    LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
  deriving (GHC.Read.Read,
            GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Text.ProtocolBuffers.Basic.Mergeable OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
instance GHC.Enum.Bounded OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  minBound = SPEED_TextDescriptorProtosFileOptionsOptimizeMode
  maxBound = LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
instance Text.ProtocolBuffers.Basic.Default OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  defaultValue = SPEED_TextDescriptorProtosFileOptionsOptimizeMode
toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode ::
  GHC.Types.Int
  -> GHC.Maybe.Maybe OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode 1
  = GHC.Maybe.Just SPEED_TextDescriptorProtosFileOptionsOptimizeMode
toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode 2
  = GHC.Maybe.Just
      CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode
toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode 3
  = GHC.Maybe.Just
      LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode _
  = GHC.Maybe.Nothing
instance GHC.Enum.Enum OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  fromEnum SPEED_TextDescriptorProtosFileOptionsOptimizeMode = 1
  fromEnum CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode = 2
  fromEnum LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
    = 3
  toEnum
    = Data.Maybe.fromMaybe
        (GHC.Err.error
           "hprotoc generated code: toEnum failure for type Text.DescriptorProtos.FileOptions.OptimizeMode")
        . toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode
  succ SPEED_TextDescriptorProtosFileOptionsOptimizeMode
    = CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode
  succ CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode
    = LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
  succ _
    = GHC.Err.error
        "hprotoc generated code: succ failure for type Text.DescriptorProtos.FileOptions.OptimizeMode"
  pred CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode
    = SPEED_TextDescriptorProtosFileOptionsOptimizeMode
  pred LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
    = CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode
  pred _
    = GHC.Err.error
        "hprotoc generated code: pred failure for type Text.DescriptorProtos.FileOptions.OptimizeMode"
instance Text.ProtocolBuffers.WireMessage.Wire OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  wireSize ft' enum
    = Text.ProtocolBuffers.WireMessage.wireSize
        ft' (GHC.Enum.fromEnum enum)
  wirePut ft' enum
    = Text.ProtocolBuffers.WireMessage.wirePut
        ft' (GHC.Enum.fromEnum enum)
  wireGet 14
    = Text.ProtocolBuffers.WireMessage.wireGetEnum
        toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode
  wireGet ft' = Text.ProtocolBuffers.WireMessage.wireGetErr ft'
  wireGetPacked 14
    = Text.ProtocolBuffers.WireMessage.wireGetPackedEnum
        toMaybe'Enum_TextDescriptorProtosFileOptionsOptimizeMode
  wireGetPacked ft' = Text.ProtocolBuffers.WireMessage.wireGetErr ft'
instance Text.ProtocolBuffers.Extensions.GPB OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
instance Text.ProtocolBuffers.Extensions.MessageAPI msg' (msg'
                                                          -> OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode) OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  getVal m' f' = f' m'
instance Text.ProtocolBuffers.Reflections.ReflectEnum OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  reflectEnum
    = [(1, "SPEED", SPEED_TextDescriptorProtosFileOptionsOptimizeMode),
       (2, "CODE_SIZE", 
        CODE_SIZE_TextDescriptorProtosFileOptionsOptimizeMode),
       (3, "LITE_RUNTIME", 
        LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode)]
  reflectEnumInfo _
    = Text.ProtocolBuffers.Reflections.EnumInfo
        (Text.ProtocolBuffers.Reflections.makePNF
           (Data.ByteString.Lazy.Char8.pack
              ".google.protobuf.FileOptions.OptimizeMode")
           ["Text"]
           ["DescriptorProtos", "FileOptions"]
           "OptimizeMode")
        ["Text", "DescriptorProtos", "FileOptions", "OptimizeMode.hs"]
        [(1, "SPEED"), (2, "CODE_SIZE"), (3, "LITE_RUNTIME")]
        GHC.Types.True
instance Text.ProtocolBuffers.TextMessage.TextType OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode where
  tellT = Text.ProtocolBuffers.TextMessage.tellShow
  getT = Text.ProtocolBuffers.TextMessage.getRead
data UninterpretedOption_TextDescriptorProtosUninterpretedOption
  = UninterpretedOption_TextDescriptorProtosUninterpretedOption {name_TextDescriptorProtosUninterpretedOption :: !(Data.Sequence.Internal.Seq NamePart_TextDescriptorProtosUninterpretedOptionNamePart),
                                                                 identifier_value_TextDescriptorProtosUninterpretedOption :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                                 positive_int_value_TextDescriptorProtosUninterpretedOption :: !(GHC.Maybe.Maybe GHC.Word.Word64),
                                                                 negative_int_value_TextDescriptorProtosUninterpretedOption :: !(GHC.Maybe.Maybe GHC.Int.Int64),
                                                                 double_value_TextDescriptorProtosUninterpretedOption :: !(GHC.Maybe.Maybe GHC.Types.Double),
                                                                 string_value_TextDescriptorProtosUninterpretedOption :: !(GHC.Maybe.Maybe Data.ByteString.Lazy.Internal.ByteString),
                                                                 aggregate_value_TextDescriptorProtosUninterpretedOption :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                                 unknown'field_TextDescriptorProtosUninterpretedOption :: !(Text.ProtocolBuffers.Unknown.UnknownField)}
  deriving (GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Text.ProtocolBuffers.Unknown.UnknownMessage UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  getUnknownField
    = unknown'field_TextDescriptorProtosUninterpretedOption
  putUnknownField u'f msg
    = msg {unknown'field_TextDescriptorProtosUninterpretedOption = u'f}
instance Text.ProtocolBuffers.Basic.Mergeable UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  mergeAppend
    (UninterpretedOption_TextDescriptorProtosUninterpretedOption x'1
                                                                 x'2
                                                                 x'3
                                                                 x'4
                                                                 x'5
                                                                 x'6
                                                                 x'7
                                                                 x'8)
    (UninterpretedOption_TextDescriptorProtosUninterpretedOption y'1
                                                                 y'2
                                                                 y'3
                                                                 y'4
                                                                 y'5
                                                                 y'6
                                                                 y'7
                                                                 y'8)
    = UninterpretedOption_TextDescriptorProtosUninterpretedOption
        (Text.ProtocolBuffers.Basic.mergeAppend x'1 y'1)
        (Text.ProtocolBuffers.Basic.mergeAppend x'2 y'2)
        (Text.ProtocolBuffers.Basic.mergeAppend x'3 y'3)
        (Text.ProtocolBuffers.Basic.mergeAppend x'4 y'4)
        (Text.ProtocolBuffers.Basic.mergeAppend x'5 y'5)
        (Text.ProtocolBuffers.Basic.mergeAppend x'6 y'6)
        (Text.ProtocolBuffers.Basic.mergeAppend x'7 y'7)
        (Text.ProtocolBuffers.Basic.mergeAppend x'8 y'8)
instance Text.ProtocolBuffers.Basic.Default UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  defaultValue
    = UninterpretedOption_TextDescriptorProtosUninterpretedOption
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
instance Text.ProtocolBuffers.WireMessage.Wire UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  wireSize
    ft'
    self'@(UninterpretedOption_TextDescriptorProtosUninterpretedOption x'1
                                                                       x'2
                                                                       x'3
                                                                       x'4
                                                                       x'5
                                                                       x'6
                                                                       x'7
                                                                       x'8)
    = case ft' of
        10 -> calc'Size
        11 -> Text.ProtocolBuffers.WireMessage.prependMessageSize calc'Size
        _ -> Text.ProtocolBuffers.WireMessage.wireSizeErr ft' self'
    where
        calc'Size
          = (Text.ProtocolBuffers.WireMessage.wireSizeRep 1 11 x'1
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 9 x'2
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 4 x'3
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 3 x'4
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 1 x'5
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 12 x'6
               + Text.ProtocolBuffers.WireMessage.wireSizeOpt 1 9 x'7
               + Text.ProtocolBuffers.Unknown.wireSizeUnknownField x'8)
  wirePutWithSize
    ft'
    self'@(UninterpretedOption_TextDescriptorProtosUninterpretedOption x'1
                                                                       x'2
                                                                       x'3
                                                                       x'4
                                                                       x'5
                                                                       x'6
                                                                       x'7
                                                                       x'8)
    = case ft' of
        10 -> put'Fields
        11 -> put'FieldsSized
        _ -> Text.ProtocolBuffers.WireMessage.wirePutErr ft' self'
    where
        put'Fields
          = Text.ProtocolBuffers.WireMessage.sequencePutWithSize
              [Text.ProtocolBuffers.WireMessage.wirePutRepWithSize 18 11 x'1,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 26 9 x'2,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 32 4 x'3,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 40 3 x'4,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 49 1 x'5,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 58 12 x'6,
               Text.ProtocolBuffers.WireMessage.wirePutOptWithSize 66 9 x'7,
               Text.ProtocolBuffers.Unknown.wirePutUnknownFieldWithSize x'8]
        put'FieldsSized
          = let
              size' = Data.Tuple.fst (Data.Binary.Put.runPutM put'Fields)
              put'Size
                = do Text.ProtocolBuffers.WireMessage.putSize size'
                     GHC.Base.return
                       (Text.ProtocolBuffers.WireMessage.size'WireSize size')
            in
              Text.ProtocolBuffers.WireMessage.sequencePutWithSize
                [put'Size, put'Fields]
  wireGet ft'
    = case ft' of
        10
          -> Text.ProtocolBuffers.WireMessage.getBareMessageWith
               (Text.ProtocolBuffers.Unknown.catch'Unknown update'Self)
        11
          -> Text.ProtocolBuffers.WireMessage.getMessageWith
               (Text.ProtocolBuffers.Unknown.catch'Unknown update'Self)
        _ -> Text.ProtocolBuffers.WireMessage.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
          = case wire'Tag of
              18
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {name_TextDescriptorProtosUninterpretedOption = Text.ProtocolBuffers.Header.append
                                                                               (name_TextDescriptorProtosUninterpretedOption
                                                                                  old'Self)
                                                                               new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 11)
              26
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {identifier_value_TextDescriptorProtosUninterpretedOption = GHC.Maybe.Just
                                                                                           new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              32
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {positive_int_value_TextDescriptorProtosUninterpretedOption = GHC.Maybe.Just
                                                                                             new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 4)
              40
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {negative_int_value_TextDescriptorProtosUninterpretedOption = GHC.Maybe.Just
                                                                                             new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 3)
              49
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {double_value_TextDescriptorProtosUninterpretedOption = GHC.Maybe.Just
                                                                                       new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 1)
              58
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {string_value_TextDescriptorProtosUninterpretedOption = GHC.Maybe.Just
                                                                                       new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 12)
              66
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {aggregate_value_TextDescriptorProtosUninterpretedOption = GHC.Maybe.Just
                                                                                          new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              _ -> let
                     (field'Number, wire'Type)
                       = Text.ProtocolBuffers.WireMessage.splitWireTag wire'Tag
                   in
                     Text.ProtocolBuffers.WireMessage.unknown
                       field'Number wire'Type old'Self
instance Text.ProtocolBuffers.Extensions.MessageAPI msg' (msg'
                                                          -> UninterpretedOption_TextDescriptorProtosUninterpretedOption) UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  getVal m' f' = f' m'
instance Text.ProtocolBuffers.Extensions.GPB UninterpretedOption_TextDescriptorProtosUninterpretedOption
instance Text.ProtocolBuffers.Reflections.ReflectDescriptor UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  getMessageInfo _
    = Text.ProtocolBuffers.Reflections.GetMessageInfo
        (Data.Set.Internal.fromDistinctAscList [])
        (Data.Set.Internal.fromDistinctAscList
           [18, 26, 32, 40, 49, 58, 66])
  reflectDescriptorInfo _
    = Text.Read.read
        "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\"], baseName = MName \"UninterpretedOption\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"UninterpretedOption.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.name\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"name\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption.NamePart\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName = MName \"NamePart\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.identifier_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"identifier_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.positive_int_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"positive_int_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 32}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 4}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.negative_int_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"negative_int_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.double_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"double_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 49}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.string_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"string_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 7}, wireTag = WireTag {getWireTag = 58}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.aggregate_value\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName' = FName \"aggregate_value\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 8}, wireTag = WireTag {getWireTag = 66}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"
instance Text.ProtocolBuffers.TextMessage.TextType UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  tellT = Text.ProtocolBuffers.TextMessage.tellSubMessage
  getT = Text.ProtocolBuffers.TextMessage.getSubMessage
instance Text.ProtocolBuffers.TextMessage.TextMsg UninterpretedOption_TextDescriptorProtosUninterpretedOption where
  textPut msg
    = do Text.ProtocolBuffers.TextMessage.tellT
           "name" (name_TextDescriptorProtosUninterpretedOption msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "identifier_value"
           (identifier_value_TextDescriptorProtosUninterpretedOption msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "positive_int_value"
           (positive_int_value_TextDescriptorProtosUninterpretedOption msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "negative_int_value"
           (negative_int_value_TextDescriptorProtosUninterpretedOption msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "double_value"
           (double_value_TextDescriptorProtosUninterpretedOption msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "string_value"
           (string_value_TextDescriptorProtosUninterpretedOption msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "aggregate_value"
           (aggregate_value_TextDescriptorProtosUninterpretedOption msg)
  textGet
    = do mods <- Text.Parsec.Combinator.sepEndBy
                   (Text.Parsec.Combinator.choice
                      [parse'name, parse'identifier_value, parse'positive_int_value,
                       parse'negative_int_value, parse'double_value, parse'string_value,
                       parse'aggregate_value])
                   Text.Parsec.Char.spaces
         GHC.Base.return
           (Data.Foldable.foldl
              (\ v f -> f v) Text.ProtocolBuffers.Basic.defaultValue mods)
    where
        parse'name
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "name"
                  GHC.Base.return
                    (\ o
                       -> o {name_TextDescriptorProtosUninterpretedOption = Text.ProtocolBuffers.Header.append
                                                                              (name_TextDescriptorProtosUninterpretedOption
                                                                                 o)
                                                                              v}))
        parse'identifier_value
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "identifier_value"
                  GHC.Base.return
                    (\ o
                       -> o {identifier_value_TextDescriptorProtosUninterpretedOption = v}))
        parse'positive_int_value
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "positive_int_value"
                  GHC.Base.return
                    (\ o
                       -> o {positive_int_value_TextDescriptorProtosUninterpretedOption = v}))
        parse'negative_int_value
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "negative_int_value"
                  GHC.Base.return
                    (\ o
                       -> o {negative_int_value_TextDescriptorProtosUninterpretedOption = v}))
        parse'double_value
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "double_value"
                  GHC.Base.return
                    (\ o
                       -> o {double_value_TextDescriptorProtosUninterpretedOption = v}))
        parse'string_value
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "string_value"
                  GHC.Base.return
                    (\ o
                       -> o {string_value_TextDescriptorProtosUninterpretedOption = v}))
        parse'aggregate_value
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "aggregate_value"
                  GHC.Base.return
                    (\ o
                       -> o {aggregate_value_TextDescriptorProtosUninterpretedOption = v}))
data NamePart_TextDescriptorProtosUninterpretedOptionNamePart
  = NamePart_TextDescriptorProtosUninterpretedOptionNamePart {name_part_TextDescriptorProtosUninterpretedOptionNamePart :: !(Text.ProtocolBuffers.Basic.Utf8),
                                                              is_extension_TextDescriptorProtosUninterpretedOptionNamePart :: !(GHC.Types.Bool),
                                                              unknown'field_TextDescriptorProtosUninterpretedOptionNamePart :: !(Text.ProtocolBuffers.Unknown.UnknownField)}
  deriving (GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Text.ProtocolBuffers.Unknown.UnknownMessage NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  getUnknownField
    = unknown'field_TextDescriptorProtosUninterpretedOptionNamePart
  putUnknownField u'f msg
    = msg
        {unknown'field_TextDescriptorProtosUninterpretedOptionNamePart = u'f}
instance Text.ProtocolBuffers.Basic.Mergeable NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  mergeAppend
    (NamePart_TextDescriptorProtosUninterpretedOptionNamePart x'1
                                                              x'2
                                                              x'3)
    (NamePart_TextDescriptorProtosUninterpretedOptionNamePart y'1
                                                              y'2
                                                              y'3)
    = NamePart_TextDescriptorProtosUninterpretedOptionNamePart
        (Text.ProtocolBuffers.Basic.mergeAppend x'1 y'1)
        (Text.ProtocolBuffers.Basic.mergeAppend x'2 y'2)
        (Text.ProtocolBuffers.Basic.mergeAppend x'3 y'3)
instance Text.ProtocolBuffers.Basic.Default NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  defaultValue
    = NamePart_TextDescriptorProtosUninterpretedOptionNamePart
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
        Text.ProtocolBuffers.Basic.defaultValue
instance Text.ProtocolBuffers.WireMessage.Wire NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  wireSize
    ft'
    self'@(NamePart_TextDescriptorProtosUninterpretedOptionNamePart x'1
                                                                    x'2
                                                                    x'3)
    = case ft' of
        10 -> calc'Size
        11 -> Text.ProtocolBuffers.WireMessage.prependMessageSize calc'Size
        _ -> Text.ProtocolBuffers.WireMessage.wireSizeErr ft' self'
    where
        calc'Size
          = (Text.ProtocolBuffers.WireMessage.wireSizeReq 1 9 x'1
               + Text.ProtocolBuffers.WireMessage.wireSizeReq 1 8 x'2
               + Text.ProtocolBuffers.Unknown.wireSizeUnknownField x'3)
  wirePutWithSize
    ft'
    self'@(NamePart_TextDescriptorProtosUninterpretedOptionNamePart x'1
                                                                    x'2
                                                                    x'3)
    = case ft' of
        10 -> put'Fields
        11 -> put'FieldsSized
        _ -> Text.ProtocolBuffers.WireMessage.wirePutErr ft' self'
    where
        put'Fields
          = Text.ProtocolBuffers.WireMessage.sequencePutWithSize
              [Text.ProtocolBuffers.WireMessage.wirePutReqWithSize 10 9 x'1,
               Text.ProtocolBuffers.WireMessage.wirePutReqWithSize 16 8 x'2,
               Text.ProtocolBuffers.Unknown.wirePutUnknownFieldWithSize x'3]
        put'FieldsSized
          = let
              size' = Data.Tuple.fst (Data.Binary.Put.runPutM put'Fields)
              put'Size
                = do Text.ProtocolBuffers.WireMessage.putSize size'
                     GHC.Base.return
                       (Text.ProtocolBuffers.WireMessage.size'WireSize size')
            in
              Text.ProtocolBuffers.WireMessage.sequencePutWithSize
                [put'Size, put'Fields]
  wireGet ft'
    = case ft' of
        10
          -> Text.ProtocolBuffers.WireMessage.getBareMessageWith
               (Text.ProtocolBuffers.Unknown.catch'Unknown update'Self)
        11
          -> Text.ProtocolBuffers.WireMessage.getMessageWith
               (Text.ProtocolBuffers.Unknown.catch'Unknown update'Self)
        _ -> Text.ProtocolBuffers.WireMessage.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
          = case wire'Tag of
              10
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {name_part_TextDescriptorProtosUninterpretedOptionNamePart = new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 9)
              16
                -> GHC.Base.fmap
                     (\ !new'Field
                        -> old'Self
                             {is_extension_TextDescriptorProtosUninterpretedOptionNamePart = new'Field})
                     (Text.ProtocolBuffers.WireMessage.wireGet 8)
              _ -> let
                     (field'Number, wire'Type)
                       = Text.ProtocolBuffers.WireMessage.splitWireTag wire'Tag
                   in
                     Text.ProtocolBuffers.WireMessage.unknown
                       field'Number wire'Type old'Self
instance Text.ProtocolBuffers.Extensions.MessageAPI msg' (msg'
                                                          -> NamePart_TextDescriptorProtosUninterpretedOptionNamePart) NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  getVal m' f' = f' m'
instance Text.ProtocolBuffers.Extensions.GPB NamePart_TextDescriptorProtosUninterpretedOptionNamePart
instance Text.ProtocolBuffers.Reflections.ReflectDescriptor NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  getMessageInfo _
    = Text.ProtocolBuffers.Reflections.GetMessageInfo
        (Data.Set.Internal.fromDistinctAscList [10, 16])
        (Data.Set.Internal.fromDistinctAscList [10, 16])
  reflectDescriptorInfo _
    = Text.Read.read
        "DescriptorInfo {descName = ProtoName {protobufName = FIName \".google.protobuf.UninterpretedOption.NamePart\", haskellPrefix = [MName \"Text\"], parentModule = [MName \"DescriptorProtos\",MName \"UninterpretedOption\"], baseName = MName \"NamePart\"}, descFilePath = [\"Text\",\"DescriptorProtos\",\"UninterpretedOption\",\"NamePart.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.NamePart.name_part\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\",MName \"NamePart\"], baseName' = FName \"name_part\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".google.protobuf.UninterpretedOption.NamePart.is_extension\", haskellPrefix' = [MName \"Text\"], parentModule' = [MName \"DescriptorProtos\",MName \"UninterpretedOption\",MName \"NamePart\"], baseName' = FName \"is_extension\", baseNamePrefix' = \"\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], descOneofs = fromList [], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = True, lazyFields = False, makeLenses = False}"
instance Text.ProtocolBuffers.TextMessage.TextType NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  tellT = Text.ProtocolBuffers.TextMessage.tellSubMessage
  getT = Text.ProtocolBuffers.TextMessage.getSubMessage
instance Text.ProtocolBuffers.TextMessage.TextMsg NamePart_TextDescriptorProtosUninterpretedOptionNamePart where
  textPut msg
    = do Text.ProtocolBuffers.TextMessage.tellT
           "name_part"
           (name_part_TextDescriptorProtosUninterpretedOptionNamePart msg)
         Text.ProtocolBuffers.TextMessage.tellT
           "is_extension"
           (is_extension_TextDescriptorProtosUninterpretedOptionNamePart msg)
  textGet
    = do mods <- Text.Parsec.Combinator.sepEndBy
                   (Text.Parsec.Combinator.choice
                      [parse'name_part, parse'is_extension])
                   Text.Parsec.Char.spaces
         GHC.Base.return
           (Data.Foldable.foldl
              (\ v f -> f v) Text.ProtocolBuffers.Basic.defaultValue mods)
    where
        parse'name_part
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "name_part"
                  GHC.Base.return
                    (\ o
                       -> o {name_part_TextDescriptorProtosUninterpretedOptionNamePart = v}))
        parse'is_extension
          = Text.Parsec.Prim.try
              (do v <- Text.ProtocolBuffers.TextMessage.getT "is_extension"
                  GHC.Base.return
                    (\ o
                       -> o {is_extension_TextDescriptorProtosUninterpretedOptionNamePart = v}))
