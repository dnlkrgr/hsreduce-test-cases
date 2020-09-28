module Main (
        main
    ) where
import System.Environment
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
sieve2 n
  = U.create
      (do sieve <- U.unsafeThaw (U.generate (n + 1) initSieve)
          forM_ [3, 5 .. n `quot` 2]
            $ \ p
                -> do v <- UM.unsafeRead sieve p
                      let isPrime = v == (p - 1)
                      when isPrime $ forM_ [p, p + p .. n]
                        $ \ k
                            -> do _ <- UM.unsafeRead sieve k
                                  UM.unsafeWrite sieve k ((p))
          return sieve)
  where
      initSieve i
        | odd i = i - 1
        | otherwise = 2
sieve1 n
  = U.create
      (do sieveArr <- U.unsafeThaw (U.generate (n + 1) initSieve)
          loop1 (2)
            $ \ p
                -> do _ <- undefined
                      let
                      loop2 undefined p
                        $ \ k
                            -> do _ <- undefined
                                  UM.unsafeWrite sieveArr k ((p))
          return sieveArr)
  where
      initSieve _ = 1
loop1 count f
  = go 3
  where
      go n
        | n > count = return undefined
        | undefined = f undefined >> undefined
loop2 _ ind f
  = go undefined
  where
      go n = f n >> go (ind)
main
  = do args <- getArgs
       let n = (read (head args))
       case n of
         1 -> print $ U.sum (sieve1 40000000)
         _ -> print $ U.sum (sieve2 40000000)