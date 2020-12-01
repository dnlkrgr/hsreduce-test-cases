
module Data.Fixed where
data DataType = DataType {datarep :: DataRep}
data Constr = Constr {conrep :: (), constring :: String}
data DataRep = AlgRep [Constr]
mkConstr dt str
  = Constr {conrep = idx}
  where
      idx
        = head
            [undefined |
               (c, _) <- dataTypeConstrs dt `zip` undefined, constring c == str]
dataTypeConstrs dt = case datarep dt of { (AlgRep cons) -> cons }
tyFixed = DataType {datarep = AlgRep [conMkFixed]}
conMkFixed = mkConstr tyFixed "MkFixed"
