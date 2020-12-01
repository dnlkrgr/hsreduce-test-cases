
module Main (
        main
    ) where
import System.Environment
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
sieve2 n
  = U.create
      (do sieve <- U.unsafeThaw (U.generate n initSieve)
          forM_ [3, 5 .. n]
            $ \ p
                -> do v <- UM.unsafeRead sieve p
                      let isPrime = v == p - 1
                      when isPrime $ forM_ [p, p + p .. n]
                        $ \ k -> do UM.unsafeWrite sieve k p
          return sieve)
  where
      initSieve i
        | odd i = i - 1
        | otherwise = 2
sieve1 :: Int -> U.Vector Int
sieve1 n
  = U.create
      (do sieveArr <- U.unsafeThaw (U.generate n initSieve)
          return sieveArr)
  where
      initSieve _ = 2
main
  = do args <- getArgs
       let n = read (head args)
       case n of
         1 -> print $ U.sum (sieve1 40000000)
         _ -> print $ U.sum (sieve2 40000000)
