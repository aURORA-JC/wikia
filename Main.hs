{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.HashMap.Strict
import Data.Aeson
import UShow

hull = ((++) "Images/Hull/") . hull'

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

issub "Submarine"                  = True
issub "Submarine Aircraft Carrier" = True
issub _                            = False

navy = ((++) "Images/Navy/") . navy'

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
navy' "KizunaAI"            = "uwrr_icon.png" -- mistake?
navy' "Bilibili"            = "bili_icon.png"
navy' "Hololive"            = "uwrr_icon.png" -- mistake?

numbers :: Int -> H.Html
numbers n
  = H.docTypeHtml
    $ do H.head $ H.title "Natural numbers"
         H.body $ do H.p "A list of natural numbers:"
                     H.ul $ mapM_ (H.li . H.toHtml) [1 .. n]


b :: IO (Either String Value)
b = eitherDecodeFileStrict' "test"

showship x
  = do a <- (eitherDecodeFileStrict' $ "Ships/" ++ x ++ ".json" :: IO (Either String Object))
       case a of
         Left x -> putStrLn x
         Right y -> writeFile (x ++ ".html")
                    $ renderHtml
                    $ H.docTypeHtml
                    $ do H.head $ H.title (H.toHtml x)
                         H.body $ do H.h1 (H.toHtml x)
                                     showship' y

showship' x
  = do H.table $ showship'' x
       H.pre $ H.toHtml $ ushow x

showship'' x
  = do d "cn_reference"
       d "nameJP"
  where d = displayRow x

displayRow b a
  = H.tr
    $ do H.th $ H.toHtml a
         H.th $ H.toHtml $ ushow $ b ! a
