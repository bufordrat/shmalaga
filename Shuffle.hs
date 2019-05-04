{- 
   File      : Shuffle.hs

   Side-effecty function for randomly shuffling decks, for use in Galaga.hs. 

   Implementaiton of shuffle 
       @cite: https://wiki.haskell.org/Random_shuffle 
  
-}

module Shuffle
(
  shuffle, 
)
where 

import System.Random
import Data.Array.IO
import Control.Monad
import Data.List
 
-- Randomly shuffle a list
shuffle :: [a] -> IO [a]
shuffle xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
