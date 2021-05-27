#!/bin/env runhaskell

import System.Directory
import Data.List.Split
import Control.Monad
import System.Process

-- note: does not escape
main
  = do base <- getDirectoryContents "assets/painting/"
       action <- filterM (\[base, _] ->
                            doesDirectoryExist ("assets/paintingface/" ++ base))
                 $ map (endBy ".")
                 $ filter ((<) 2 . length)
                 $ filter (notElem '-') base
       mapM_ (\[base, _] -> do out <- readCreateProcess (shell
                                                          $ "compare -metric RMSE -subimage-search assets/painting/" ++ base ++ ".png assets/paintingface/" ++ base ++ "/1.png diff.png | cut -d' ' -f4 | tr ',' +") ""
                               putStrLn $ "FOR " ++ base ++ ": " ++ out
                               applyon <- getDirectoryContents ("assets/paintingface/" ++ base)
                               mapM_ (\[i, _]
                                      -> readCreateProcess (shell
                                                             $ "composite -geometry +" ++ out ++ " assets/paintingface/" ++ base ++ "/" ++ i ++ ".png assets/painting/" ++ base ++ ".png assets/painting/" ++ base ++ "-" ++ i ++ ".png") "")
                                 $ map (endBy ".")
                                 $ filter ((<) 2 . length) applyon)
         $ action
