import System.Environment 
import Control.Monad 
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
a f = 
    U.create (
    do 
      b <- U.unsafeThaw (U.generate f  c)
      forM_ [] $ \d -> do
        e <- UM.unsafeRead b d
        let g = e == d  
        forM_ [] $ \h ->  h  
      return b
    )
    where
      c 
          i = 2
j f = 
    U.create (
    do 
      k <- U.unsafeThaw (U.generate f  c)
      return k
    )
    where    
      c go =    go 
main = 
    do
      args <- getArgs
      let f  = read (head args)  
      case f of
        1 -> print $ U.sum (j 40000000)                    
        2 -> print  (a 40000000)
