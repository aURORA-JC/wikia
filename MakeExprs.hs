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
                                                          $ "compare -metric MAE -subimage-search assets/painting/" ++ base ++ ".png assets/paintingface/" ++ base ++ "/1.png diff.png 2>&1 >/dev/null | egrep -o '[0-9]+,[0-9]+' | tr ',' +") ""
                               putStrLn $ "FOR " ++ base ++ ": |" ++ (init out) ++ "|"
                               applyon <- getDirectoryContents ("assets/paintingface/" ++ base)
                               mapM_ (\[i, _]
                                      -> readCreateProcess (shell
                                                             $ "composite -geometry +" ++ (init out) ++ " assets/paintingface/" ++ base ++ "/" ++ i ++ ".png assets/painting/" ++ base ++ ".png assets/painting/" ++ base ++ "-" ++ i ++ ".png") "")
                                 $ map (endBy ".")
                                 $ filter ((<) 2 . length) applyon)
         $ action
