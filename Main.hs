{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Data.HashMap.Strict
import qualified Data.Aeson as Aeson
import System.Directory (listDirectory)
import Data.Text (unpack)
import Data.List (sort)

hull = ((++) "/images/hull/") . hull'

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

navy = ((++) "images/navy/") . navy'

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

noimg x = x

mkhtml prefix name x
  = writeFile (prefix ++ name ++ ".html")
    $ renderHtml
    $ H.docTypeHtml
    $ do H.head $ H.title (H.toHtml name)
         H.body $ do H.h1 (H.toHtml name)
                     x

showship x
  = do name <- return $ "Ships/" ++ x
       a <- (Aeson.eitherDecodeFileStrict' name :: IO (Either String Aeson.Object))
       case a of
         Left e -> error $ name ++ ": " ++ e
         Right y -> mkhtml "out/ships/" (ashow $ y ! "name_reference") $ showship' y
       return x

showship' x
  = do H.table
         $ do d "cn_reference"
              d "nameJP"
  where d = displayRow x

displayRow b a
  = H.tr
    $ do H.th $ H.toHtml a
         H.th $ H.toHtml $ ashow $ b ! a

ashow :: Aeson.Value -> String
ashow (Aeson.String x) = unpack x
ashow (Aeson.Number x) = show x
ashow (Aeson.Bool x) = show x
ashow x = show x

makeMainIndex ships
  = mkhtml "out/" "index" $ H.ol $ mapM_ (\x -> H.li $ H.a H.! A.href (H.stringValue $ "ships/" ++ x) $ H.toHtml x) ships

makeIndex x y z
  = return ()

main :: IO ()
main
  = do dir <- listDirectory "Ships"
       ships <- mapM showship $ sort dir
       makeMainIndex ships
       makeIndex "hull" hull ships
       makeIndex "navy" navy ships
       makeIndex "class" noimg ships
       return ()
