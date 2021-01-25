#!/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.Directory (listDirectory)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict ((!), elems, keys)
import Data.Text (unpack)
import Utils

wget x subdir
  = "wget https://media.alg-wiki.com/assets/" ++ subdir ++ "/" ++ x ++ ".png -Oassets/" ++ subdir ++ "/" ++ x ++ ".png"

wget' x y
  = "wget https://media.alg-wiki.com/assets/paintingface/" ++ x ++ "/" ++ y ++ ".png -Oassets/paintingface/" ++ x ++ "/" ++ y ++ ".png"


skindl :: [String]
skindl = ["painting", "squareicon", "shipmodels", "herohrzicon"]

handleship name
  = do a <- (Aeson.eitherDecodeFileStrict' ("Ships/" ++ name) :: IO (Either String Aeson.Object))
       return $ case a of
                  Left e -> error $ name ++ ": " ++ e
                  Right json -> let chname = ashow $ json ! "cn_reference"
                                    skins = join $ map (\x -> let skinname = ashow $ (aobj x) ! "id"
                                                                  expressions = keys $ aobj $ (aobj x) ! "expression"
                                                              in
                                                                map (wget skinname) skindl ++ map (wget' skinname . unpack) expressions) $ elems $ aobj $ json ! "skin"
                                    skills = map (\x -> wget (ashow $ (aobj x) ! "icon") "skillicon_new") $ elems $ aobj $ json ! "skill"
                                in
                                  [wget chname "squareicon"] ++ skins ++ skills

main :: IO ()
main
  = do putStrLn "#!/bin/sh"
       putStrLn "mkdir -p assets/paintingface"
       putStrLn "mkdir -p assets/skillicon_new"
       putStr $ unlines $ map (\x -> "mkdir -p assets/" ++ x) skindl
       dir <- listDirectory "Ships"
       out <- mapM handleship dir
       putStrLn $ unlines $ join out
