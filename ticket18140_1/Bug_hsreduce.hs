
module AllInOne (
    ) where
import Text.ProtocolBuffers.Basic
import Text.ProtocolBuffers.Extensions
import Text.ProtocolBuffers.Unknown
data FileOptions_TextDescriptorProtosFileOptions
  = FileOptions_TextDescriptorProtosFileOptions {java_package_TextDescriptorProtosFileOptions :: !(Maybe Utf8),
                                                 java_outer_classname_TextDescriptorProtosFileOptions :: !(Maybe Utf8),
                                                 java_multiple_files_TextDescriptorProtosFileOptions :: !(Maybe Bool),
                                                 java_generate_equals_and_hash_TextDescriptorProtosFileOptions :: !(Maybe Bool),
                                                 java_string_check_utf8_TextDescriptorProtosFileOptions :: !(Maybe Bool),
                                                 optimize_for_TextDescriptorProtosFileOptions :: (),
                                                 go_package_TextDescriptorProtosFileOptions :: (),
                                                 cc_generic_services_TextDescriptorProtosFileOptions :: (),
                                                 java_generic_services_TextDescriptorProtosFileOptions :: (),
                                                 py_generic_services_TextDescriptorProtosFileOptions :: (),
                                                 deprecated_TextDescriptorProtosFileOptions :: (),
                                                 cc_enable_arenas_TextDescriptorProtosFileOptions :: !(Maybe Bool),
                                                 objc_class_prefix_TextDescriptorProtosFileOptions :: !(Maybe Utf8),
                                                 csharp_namespace_TextDescriptorProtosFileOptions :: !(Maybe Utf8),
                                                 javanano_use_deprecated_package_TextDescriptorProtosFileOptions :: !(Maybe Bool),
                                                 uninterpreted_option_TextDescriptorProtosFileOptions :: (),
                                                 ext'field_TextDescriptorProtosFileOptions :: !(ExtField),
                                                 unknown'field_TextDescriptorProtosFileOptions :: !(UnknownField)}
instance Mergeable FileOptions_TextDescriptorProtosFileOptions where
  mergeAppend
    (FileOptions_TextDescriptorProtosFileOptions x'1
                                                 x'2
                                                 x'3
                                                 x'4
                                                 x'5
                                                 _
                                                 _
                                                 _
                                                 _
                                                 _
                                                 _
                                                 x'12
                                                 x'13
                                                 x'14
                                                 x'15
                                                 _
                                                 x'17
                                                 x'18)
    (FileOptions_TextDescriptorProtosFileOptions y'1
                                                 y'2
                                                 y'3
                                                 y'4
                                                 y'5
                                                 _
                                                 _
                                                 _
                                                 _
                                                 _
                                                 _
                                                 y'12
                                                 y'13
                                                 y'14
                                                 y'15
                                                 _
                                                 y'17
                                                 y'18)
    = FileOptions_TextDescriptorProtosFileOptions
        (mergeAppend x'1 y'1)
        (mergeAppend x'2 y'2)
        (mergeAppend x'3 y'3)
        (mergeAppend x'4 y'4)
        (mergeAppend x'5 y'5)
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        (mergeAppend x'12 y'12)
        (mergeAppend x'13 y'13)
        (mergeAppend x'14 y'14)
        (mergeAppend x'15 y'15)
        undefined
        (mergeAppend x'17 y'17)
        (mergeAppend x'18 y'18)
instance Default FileOptions_TextDescriptorProtosFileOptions
