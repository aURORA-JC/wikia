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
                 $ filter (notElem '-')
                 $ base
       action' <- filterM (\[base, _] ->
                             doesFileExist ("assets/paintingface/" ++ base ++ "/0.png") >>= return . not) action
       mapM_ (\[base, _]
              -> do size <- readCreateProcess (shell
                                                $ "identify -format %wx%h assets/paintingface/"
                                                ++ base
                                                ++ "/1.png") ""
                    out <- readCreateProcess (shell
                                              $ "compare -metric RMSE -subimage-search assets/painting/"
                                              ++ base
                                              ++ ".png assets/paintingface/"
                                              ++ base
                                              ++ "/1.png diff.png 2>&1 >/dev/null | egrep -o '[0-9]+,[0-9]+' | tr ',' +") ""
                    putStrLn $ "FOR " ++ base ++ ": |" ++ (init out) ++ "|"
                    readCreateProcess (shell
                                        $ "convert assets/painting/" ++ base ++ ".png -crop " ++ size ++ "+" ++ (init out) ++ " assets/paintingface/" ++ base ++ "/0.png ") "")
         $ action'
