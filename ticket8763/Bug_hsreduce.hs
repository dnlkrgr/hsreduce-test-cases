{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
module Main (
        main
    ) where
import Prelude hiding ( read )
import Control.Monad
import Data.Vector ( (!) )
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Vector.Mutable ( STVector, new, read, write )
_SIZE = 512
a = V.replicate _SIZE (U.replicate _SIZE 1)
c = V.create
      $ do v :: STVector s (UM.STVector s Int) <- new _SIZE
           forM_ [0 .. _SIZE - 1]
             $ \ i
                 -> do w <- UM.replicate _SIZE 0
                       write v i w
           loop _SIZE
             $ \ i
                 -> do y <- read v i
                       loop _SIZE
                         $ \ k
                             -> do loop _SIZE
                                     $ \ j
                                         -> do x <- UM.unsafeRead y j
                                               UM.unsafeWrite y j $! x + (a ! i !. j) * (a ! k !. j)
(!.) = U.unsafeIndex
loop bex f
  = go 0
  where
      go !n
        | n == bex = return undefined
        | otherwise = f n >> go (n + 1)
res = V.sum (V.map undefined c)
main = print res
