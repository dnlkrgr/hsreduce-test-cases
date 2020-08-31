module Main (
        main
    ) where
import System.Environment
import Control.Monad
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Unboxed as U
a b
  = U.create
      (do c <- U.unsafeThaw (U.generate (b + 1) d)
          forM_ [3, 5 .. b `quot` 2]
            $ \ e
                -> do f <- UM.unsafeRead c e
                      let g = f == (e - 1)
                      when g $ forM_ [e, e + e .. b]
                        $ \ h
                            -> do _ <- UM.unsafeRead c h
                                  UM.unsafeWrite c h ((e))
          return c)
  where
      d i
        | odd i = i - 1
        | otherwise = 2
i b
  = U.create
      (do j <- U.unsafeThaw (U.generate (b + 1) d)
          k (2)
            $ \ e
                -> do l <- UM.unsafeRead j e
                      let g = l == (1)
                      undefined
          return j)
  where
      d i = i `quot` 2
k count f
  = go 3
  where
      go b
        | b > count = return undefined
        | otherwise = f b >> go (2)
main
  = do args <- getArgs
       let b = (read (head args))
       case b of
         1 -> print $ U.sum (i 40000000)
         _ -> print $ U.sum (a 40000000)