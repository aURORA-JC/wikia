#!/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import System.Directory (listDirectory)
import qualified Data.Aeson as Aeson
import Data.HashMap.Strict ((!), elems, keys)
import Data.Text (unpack)
import Text.Read (readEither)
import Utils

wget x subdir
  = "wget https://media.alg-wiki.com/assets/" ++ subdir ++ "/" ++ x ++ ".png -Oassets/" ++ subdir ++ "/" ++ x ++ ".png"

wget' x y
  = "wget https://media.alg-wiki.com/assets/paintingface/" ++ x ++ "/" ++ y ++ ".png -Oassets/paintingface/" ++ x ++ "/" ++ y ++ ".png"

wgetout x subdir y
  = "wget https://media.alg-wiki.com/assets/" ++ subdir ++ "/" ++ x ++ ".png -Oassets/" ++ subdir ++ "/" ++ y ++ ".png"


skindl :: [String]
skindl = ["painting",
          "squareicon",
          "shipmodels",
          "herohrzicon",
          "shipyardicon_new"]

handleship name
  = do a <- (Aeson.eitherDecodeFileStrict' ("Ships/" ++ name) :: IO (Either String Aeson.Object))
       return $ case a of
                  Left e -> error $ name ++ ": " ++ e
                  Right json -> let chname = ashow $ json ! "cn_reference"
                                    skins = join $ map (\x -> let skinname = ashow $ (aobj x) ! "id"
                                                                  expressions = keys $ aobj $ (aobj x) ! "expression"
                                                              in
                                                                map (wget skinname) skindl ++ ["mkdir -p assets/paintingface/" ++ skinname] ++ map (wget' skinname . unpack) expressions) $ elems $ aobj $ json ! "skin"
                                    skills = map (\x -> wgetout (show $ floor ((case readEither $ ashow $ (aobj x) ! "icon" :: Either String Double of
                                                                                  Left s -> 0.0
                                                                                  Right x | x >= 20000.0 && x < 29000.0 -> let tempid = (fromIntegral $ floor (x / 100.0)) * 100.0 :: Double
                                                                                                                           in
                                                                                                                             tempid - ((fromIntegral $ floor (tempid / 1000.0)) * 1000.0 - 20000.0)
                                                                                  Right x -> x) / 10.0) * 10) "skillicon_new" $ ashow $ (aobj x) ! "icon") $ elems $ aobj $ json ! "skill"
                                in
                                  ["#file: " ++ name, wget chname "squareicon"] ++ skins ++ skills ++ ["#special bullin skill handling", wget "14" "skillicon_new", wget "15" "skillicon_new"]

main :: IO ()
main
  = do putStrLn "#!/bin/sh"
       putStrLn "mkdir -p assets/paintingface"
       putStrLn "mkdir -p assets/skillicon_new"
       putStr $ unlines $ map (\x -> "mkdir -p assets/" ++ x) skindl
       dir <- listDirectory "Ships"
       out <- mapM handleship dir
       putStrLn $ unlines $ join out
