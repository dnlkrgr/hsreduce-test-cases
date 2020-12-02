
module AllInOne (
    ) where
import GHC.Maybe
data FileOptions_TextDescriptorProtosFileOptions_AllInOne
  = FileOptions_TextDescriptorProtosFileOptions_AllInOne !(Maybe ()) !(Maybe ()) () !(Maybe ()) !(Maybe ()) () !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) !(Maybe ()) () () () ()
instance Mergeable_TextProtocolBuffersBasic FileOptions_TextDescriptorProtosFileOptions_AllInOne where
  mergeAppend_TextProtocolBuffersBasic
    (FileOptions_TextDescriptorProtosFileOptions_AllInOne x'1
                                                          x'2
                                                          _
                                                          x'4
                                                          x'5
                                                          _
                                                          x'7
                                                          x'8
                                                          x'9
                                                          x'10
                                                          x'11
                                                          _
                                                          _
                                                          _
                                                          _)
    (FileOptions_TextDescriptorProtosFileOptions_AllInOne y'1
                                                          y'2
                                                          _
                                                          y'4
                                                          y'5
                                                          _
                                                          y'7
                                                          y'8
                                                          y'9
                                                          y'10
                                                          y'11
                                                          _
                                                          _
                                                          _
                                                          _)
    = FileOptions_TextDescriptorProtosFileOptions_AllInOne
        (mergeAppend_TextProtocolBuffersBasic x'1 y'1)
        (mergeAppend_TextProtocolBuffersBasic x'2 y'2)
        undefined
        (mergeAppend_TextProtocolBuffersBasic x'4 y'4)
        (mergeAppend_TextProtocolBuffersBasic x'5 y'5)
        undefined
        (mergeAppend_TextProtocolBuffersBasic x'7 y'7)
        (mergeAppend_TextProtocolBuffersBasic x'8 y'8)
        (mergeAppend_TextProtocolBuffersBasic x'9 y'9)
        (mergeAppend_TextProtocolBuffersBasic x'10 y'10)
        (mergeAppend_TextProtocolBuffersBasic x'11 y'11)
        undefined
        undefined
        undefined
        undefined
class Mergeable_TextProtocolBuffersBasic a where
  mergeAppend_TextProtocolBuffersBasic :: a -> a -> a
instance Mergeable_TextProtocolBuffersBasic (GHC.Maybe.Maybe a) where
  mergeAppend_TextProtocolBuffersBasic
    = mayMerge_TextProtocolBuffersBasic
mayMerge_TextProtocolBuffersBasic GHC.Maybe.Nothing y = y
mayMerge_TextProtocolBuffersBasic x GHC.Maybe.Nothing = x
mayMerge_TextProtocolBuffersBasic _ _ = GHC.Maybe.Just undefined
