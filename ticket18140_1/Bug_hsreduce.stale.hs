{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
module AllInOne (
    ) where
import qualified Data.Data
import qualified Data.Typeable
import qualified GHC.Classes
import qualified GHC.Generics
import qualified GHC.Maybe
import qualified GHC.Read
import qualified GHC.Show
import qualified GHC.Types
import qualified Text.ProtocolBuffers.Basic
data FileOptions_TextDescriptorProtosFileOptions
  = FileOptions_TextDescriptorProtosFileOptions {java_generate_equals_and_hash_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 java_string_check_utf8_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 optimize_for_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode),
                                                 go_package_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe Text.ProtocolBuffers.Basic.Utf8),
                                                 cc_generic_services_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 java_generic_services_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 py_generic_services_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 deprecated_TextDescriptorProtosFileOptions :: !(GHC.Maybe.Maybe GHC.Types.Bool),
                                                 cc_enable_arenas_TextDescriptorProtosFileOptions :: (),
                                                 objc_class_prefix_TextDescriptorProtosFileOptions :: (),
                                                 csharp_namespace_TextDescriptorProtosFileOptions :: (),
                                                 javanano_use_deprecated_package_TextDescriptorProtosFileOptions :: (),
                                                 uninterpreted_option_TextDescriptorProtosFileOptions :: (),
                                                 ext'field_TextDescriptorProtosFileOptions :: (),
                                                 unknown'field_TextDescriptorProtosFileOptions :: ()}
  deriving (GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Text.ProtocolBuffers.Basic.Mergeable FileOptions_TextDescriptorProtosFileOptions where
  mergeAppend
    (FileOptions_TextDescriptorProtosFileOptions x'4
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
    (FileOptions_TextDescriptorProtosFileOptions y'4
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
    = FileOptions_TextDescriptorProtosFileOptions
        (Text.ProtocolBuffers.Basic.mergeAppend x'4 y'4)
        (Text.ProtocolBuffers.Basic.mergeAppend x'5 y'5)
        (Text.ProtocolBuffers.Basic.mergeAppend x'6 y'6)
        (Text.ProtocolBuffers.Basic.mergeAppend x'7 y'7)
        (Text.ProtocolBuffers.Basic.mergeAppend x'8 y'8)
        (Text.ProtocolBuffers.Basic.mergeAppend x'9 y'9)
        (Text.ProtocolBuffers.Basic.mergeAppend x'10 y'10)
        (Text.ProtocolBuffers.Basic.mergeAppend x'11 y'11)
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
instance Text.ProtocolBuffers.Basic.Default FileOptions_TextDescriptorProtosFileOptions
data OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
  = LITE_RUNTIME_TextDescriptorProtosFileOptionsOptimizeMode
  deriving (GHC.Read.Read,
            GHC.Show.Show,
            GHC.Classes.Eq,
            GHC.Classes.Ord,
            Data.Typeable.Typeable,
            Data.Data.Data,
            GHC.Generics.Generic)
instance Text.ProtocolBuffers.Basic.Mergeable OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
instance Text.ProtocolBuffers.Basic.Default OptimizeMode_TextDescriptorProtosFileOptionsOptimizeMode
