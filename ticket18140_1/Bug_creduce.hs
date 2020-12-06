module AllInOne where
data FileOptions_TextDescriptorProtosFileOptions_AllInOne
  = FileOptions_TextDescriptorProtosFileOptions_AllInOne {b ,
                                                          c ,
                                                          d ,
                                                          e ,
                                                          f ,
                                                          g ,
                                                          h ,
                                                          i ,
                                                          s ,
                                                          j ,
                                                          k :: !(Maybe Bool),
                                                          l ,
                                                          m ,
                                                          q ,
                                                          n ,
                                                          o ,
                                                          ext'field_TextDescriptorProtosFileOptions_AllInOne :: (),
                                                          unknown':: ()}
instance Mergeable_TextProtocolBuffersBasic FileOptions_TextDescriptorProtosFileOptions_AllInOne where
  p
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
        (p x'1 y'1)
        (p x'2 y'2)
        (p x'3 y'3)
        (p x'4 y'4)
        (p x'5 y'5)
        (p x'6 y'6)
        (p x'7 y'7)
        (p x'8 y'8)
        (p x'9 y'9)
        (p x'10 y'10)
        (p x'11 y'11)
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
        undefined
class Mergeable_TextProtocolBuffersBasic a where
  p :: a -> a -> a
instance Mergeable_TextProtocolBuffersBasic (Maybe a) where
  p
    = r
r Nothing y = y
r x Nothing = x
r
   x
  (Just y)
  = Just y
