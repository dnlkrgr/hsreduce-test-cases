module Data.Fixed (
        mkConstr, conMkFixed
    ) where
data DataType = DataType {datarep :: DataRep}
data Constr = Constr {conrep :: (), constring :: String}
data DataRep = AlgRep [Constr] | IntRep
mkDataType _ cs = DataType {datarep = AlgRep cs}
mkConstr dt str _ _
  = Constr {conrep = idx}
  where
      idx
        = head
            [undefined |
               (c, _) <- dataTypeConstrs dt `zip` undefined, showConstr c == str]
dataTypeConstrs dt
  = case datarep dt of
      (AlgRep cons) -> cons
      _ -> undefined ++ undefined
showConstr = constring
tyFixed = mkDataType undefined [conMkFixed]
conMkFixed = mkConstr tyFixed "MkFixed" undefined undefined