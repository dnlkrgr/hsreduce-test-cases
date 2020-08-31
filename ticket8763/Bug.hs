{-# OPTIONS_GHC -Wall #-}
-- both sieve1 and sieve2 run in about the same time now in 8.4.1 but sieve2, the forM_ version allocates 50% more

module Main  (main) where 

import System.Environment (getArgs)
import Control.Monad (when, forM_)
import GHC.ST
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U

--if a the returned vector then a ! i = number of numbers < i that are relatively prime to i
sieve2 :: Int -> U.Vector Int     
sieve2 n = 
    U.create (
    do 
      sieve <- U.unsafeThaw (U.generate (n + 1) initSieve)
      -- performance issue here, see https://ghc.haskell.org/trac/ghc/ticket/8763
      -- twice as slow as it should be
      forM_ [3, 5 ..n `quot` 2] $ \p -> do
        v <- UM.unsafeRead sieve p
        let isPrime = v == (p - 1)
        when isPrime $ 
          forM_ [p + p, p + p + p .. n] $ \k ->  do
            v' <- UM.unsafeRead sieve k
            --need a comment explaining why this works
            UM.unsafeWrite sieve k  (v' - (v' `quot` p))
      return sieve
    )
    where
      initSieve 0 = 0
      initSieve 1 = 1
      initSieve i
         | odd i     = i - 1 -- assume prime
         | otherwise = i `quot` 2


sieve1 :: Int -> U.Vector Int     
sieve1 n = 
    U.create (
    do 
      sieveArr <- U.unsafeThaw (U.generate (n + 1) initSieve)
      loop1 (n `quot` 2) $ \ p -> do
        val <- UM.unsafeRead sieveArr p
        let isPrime = val == (p - 1)
        when isPrime $ 
          loop2 n p $ \ k ->  do
            v <- UM.unsafeRead sieveArr k
            --need a comment explaining why this works
            UM.unsafeWrite sieveArr k  (v - (v `quot` p))
      return sieveArr
    )
    where    
      initSieve 0 = 0
      initSieve 1 = 1
      initSieve i
          | odd i     = i - 1
          | otherwise = i `quot` 2

{-# INLINE loop1 #-}    
loop1 :: Int -> (Int -> ST s ()) ->  ST s ()    
loop1 count f = go 3
  where
    go n | n > count = return ()
         | otherwise = f n >> go (n + 2)


     
{-# INLINE loop2 #-}    
loop2 :: Int -> Int -> (Int -> ST s ()) ->  ST s ()          
loop2 count ind f = go (ind + ind)
  where
    go n | n > count = return ()
         | otherwise = f n >> go (n + ind)
           
-- with arg   40000000 
-- ans is 486341683267690
main :: IO ()
main = 
    do
      args <- getArgs
      let n  = (read (head args)) :: Int
      case n of
        1 -> print $ U.sum (sieve1 40000000)                    
        2 -> print $ U.sum (sieve2 40000000) 
        _ -> error "arg must be '1' or '2'"
