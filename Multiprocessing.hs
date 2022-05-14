module Multiprocessing where

import Control.Concurrent

split :: Int -> [a] -> [[a]]
split n
  = split' [] (replicate n [])
  where split' :: [[a]] -> [[a]] -> [a] -> [[a]]
        split' xs  (y:ys) (z:zs) = split' ((z:y):xs) ys zs
        split' xs  []     zs     = split' [] xs zs
        split' ret ret2   []     = ret ++ ret2

parMap :: Int -> (x -> IO ()) -> [x] -> IO ()
parMap n f x
  = mapM_ (\x -> forkOS $ mapM_ f x) $ split n x
