#!/bin/env runhaskell

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Prelude (IO, Either(Left, Right), String, return, mapM, ($), (++), mapM_, show, error, Bool(False, True), (.), writeFile, const, id, readFile, (==), snd, Int, read)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.HashMap.Strict ((!))
import qualified Data.Aeson as Aeson
import System.IO.Error (catchIOError)
import System.Directory (listDirectory, removeDirectoryRecursive, createDirectory)
import Data.Text (pack, unpack)
import Data.List (sort, sortOn, sortBy, groupBy, head)
import Text.Read (readEither)
import Data.Ord (comparing)

(!!) :: (H.Html -> H.Html)
     -> H.Attribute
     -> H.Html
     -> H.Html
(!!) = (H.!)

hull :: String
     -> String
hull = ((++) "/images/hull/") . hull'

hull' :: String
      -> String
hull' "Aircraft Carrier"           = "cv.png"
hull' "Light Aircraft Carrier"     = "cvl.png"
hull' "Destroyer"                  = "dd.png"
hull' "Light Cruiser"              = "cl.png"
hull' "Heavy Cruiser"              = "ca.png"
hull' "Super Cruiser"              = "cb.png"
hull' "Battleship"                 = "bb.png"
hull' "Battlecruiser"              = "bc.png"
hull' "Monitor"                    = "bm.png"
hull' "Aviation Battleship"        = "bbv.png"
hull' "Submarine"                  = "ss.png"
hull' "Submarine Aircraft Carrier" = "ssv.png"
hull' "Repair Ship"                = "ar.png"
hull' "Repair"                     = "ar.png"

issub :: String
      -> Bool
issub "Submarine"                  = True
issub "Submarine Aircraft Carrier" = True
issub _                            = False

navy :: String
     -> String
navy = ((++) "images/navy/") . navy'

navy' :: String
      -> String
navy' "Eagle Union"         = "uss_icon.png"
navy' "Sakura Empire"       = "ijn_icon.png"
navy' "Royal Navy"          = "hms_icon.png"
navy' "Sardegna Empire"     = "rn_icon.png"
navy' "Ironblood"           = "kms_icon.png"
navy' "Dragon Empery"       = "roc_icon.png"
navy' "Eastern Radiance"    = "roc_icon.png"
navy' "Dragon Empire"       = "roc_icon.png"
navy' "Northern Parliament" = "sn_icon.png"
navy' "North Union"         = "sn_icon.png"
navy' "Northern Union"      = "sn_icon.png"
navy' "Vichya Dominion"     = "mnf_icon.png"
navy' "Iris Libre"          = "ffnf_icon.png"
navy' "Universal"           = "univ_icon.png"
navy' "Neptunia"            = "hdn_icon.png"
navy' "Utawarerumono"       = "uwrr_icon.png"
navy' "KizunaAI"            = "uwrr_icon.png"
navy' "Bilibili"            = "bili_icon.png"
navy' "Hololive"            = "uwrr_icon.png"

mkhtml prefix name title x
  = do css <- readFile "main.css"
       writeFile (prefix ++ name ++ ".html")
         $ renderHtml
         $ H.docTypeHtml
         $ do H.head
                $ do H.title (H.toHtml name)
                     H.style !! A.type_ "text/css" $ H.toHtml css
              H.body x

showship x
  = do name <- return $ "Ships/" ++ x
       a <- (Aeson.eitherDecodeFileStrict' name :: IO (Either String Aeson.Object))
       case a of
         Left e -> error $ name ++ ": " ++ e
         Right json -> do name <- return $ ashow $ json ! "name_reference"
                          mkhtml "out/ships/" name (ashow $ json ! "name") $ showship' json
                          return (name, json)

showship' :: Aeson.Object
          -> H.Html
showship' json
  = do H.table
         $ do H.thead
                $ do H.tr $ H.th !! A.scope "col" !! A.colspan "2" $ H.toHtml $ ashow $ json ! "name"
                     H.tr
                       $ do H.th !! A.scope "col" $ "Property"
                            H.th !! A.scope "col" $ "Value"
              H.tbody
                $ do d "ID"          "ID"
                     d "hull"        "Hull"
                     d "navy"        "Navy"
                     d "initialStar" "Initial Rating"
                     d "rarity"      "Rarity"
  where d = displayRow json

displayRow :: Aeson.Object
           -> String
           -> String
           -> H.Html
displayRow json field text
  = H.tr
    $ do H.th !! A.scope "row" $ H.toHtml text
         H.th $ H.toHtml $ ashow $ json ! pack field

ashow :: Aeson.Value
      -> String
ashow (Aeson.String x) = unpack x
ashow (Aeson.Number x) = show x
ashow (Aeson.Bool x) = show x
ashow x = show x

makeMainIndex :: String
              -> String
              -> [(String, Aeson.Object)]
              -> IO ()
makeMainIndex file title ships
  = mkhtml "out/" file title
    $ H.ol
    $ mapM_ (\(name, json) -> H.li $ H.a !! A.href (H.stringValue $ "ships/" ++ name ++ ".html") $ H.toHtml name) ships

makeIndex :: String
          -> (String -> String)
          -> [(String, Aeson.Object)]
          -> IO ()
makeIndex category f ships
  = do createDirectory $ "out/" ++ category
       mapM_ (\x -> do name <- return $ ashow $ (snd $ head x) ! (pack category)
                       mkhtml ("out/" ++ category ++ "/") name name
                         $ H.ol
                         $ mapM_ (\(name, json) -> H.li $ H.a !! A.href (H.stringValue $ "../ships/" ++ name ++ ".html") $ H.toHtml name) x) groupedShips
  where
    groupedShips :: [[(String, Aeson.Object)]]
    groupedShips = groupBy (\(_, a) -> \(_, b) -> a ! (pack category) == b ! (pack category)) $ sortBy (comparing $ \a -> ashow $ (snd a) ! (pack category)) ships

main :: IO ()
main
  = do catchIOError (removeDirectoryRecursive "out") $ const $ return ()
       createDirectory "out"
       createDirectory "out/ships"
       dir <- listDirectory "Ships"
       ships <- mapM showship $ sort dir
       makeMainIndex "index_alphabetic" "Index (Alphabetic)" ships
       makeMainIndex "index_id"         "Index (By ID)"
         $ sortOn (\(_, json) -> case readEither (ashow $ json ! "ID") :: Either String Int of
                                   Left _ -> 0
                                   Right x -> x) ships
       makeIndex "rarity" id   ships
       makeIndex "hull"   hull ships
       makeIndex "navy"   navy ships
       return ()
