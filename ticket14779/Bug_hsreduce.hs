module Data.Fixed (
        a, b
    ) where
data C = C {d :: DataRep}
data Constr = Constr {conrep :: ConstrRep, constring :: String}
data DataRep = AlgRep [Constr] | E
data ConstrRep = AlgConstr ()
f _ cs = C {d = AlgRep cs}
a dt g _ _
  = Constr {conrep = AlgConstr h}
  where
      h = head
            [undefined |
               (c, _) <- dataTypeConstrs dt `zip` undefined, i c == g]
dataTypeConstrs dt = case d dt of { (AlgRep cons) -> cons }
i = constring
j = f undefined [b]
b = a j "MkFixed" undefined undefined