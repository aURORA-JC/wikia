#!/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import Data.HashMap.Strict hiding (lookup, map, filter)
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import System.IO.Error
import System.Directory
import Data.Text (pack, unpack)
import Data.List
import Text.Read
import Data.Ord
import Utils
import Debug.Trace
import Data.Char
import Data.List.Split
import Data.Maybe

(%) a b = ashow $ a ! (pack b)
(%%) :: Aeson.Object
     -> String
     -> H.Html
(%%) a b = H.preEscapedToHtml $ ashow $ a ! (pack b)

rarity :: String
     -> String
rarity = ((++) "https://algwiki.moe/Images/Rarity/") . rarity'

rarity' :: String
      -> String
rarity' "Ultra Rare" = "UR.png"
rarity' "Decisive"   = "DR.png"
rarity' "Priority"   = "PR.png"
rarity' "Super Rare" = "SSR.png"
rarity' "Elite"      = "SR.png"
rarity' "Rare"       = "R.png"
rarity' "Common"     = "C.png"
rarity' "unknown" = ""
rarity' x = error x

hull :: String
     -> String
hull = ((++) "https://algwiki.moe/Images/Hull/") . hull'

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
hull' "Munition Ship"              = "ae.png"
hull' x = error x

navy :: String
     -> String
navy = ((++) "https://algwiki.moe/Images/Navy/") . navy'

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
navy' "META"                = "meta_icon.png"
navy' "Venus Vacation"      = "uwrr_icon.png"
navy' x = error x

sidebar
  = H.nav H.! A.id "sidebar"
    $ H.ol
    $ do H.li H.! A.class_ "subheader"                                   $ H.h1 "/alg/ Wiki"

         H.li H.! A.class_ "subheader"                                   $ "Database"
         H.li $ H.a H.! A.href "https://algwiki.moe/shiplist.html"       $ "Shiplist (By ID)"
         H.li $ H.a H.! A.href "https://algwiki.moe/shiplist_alpha.html" $ "Shiplist (By Name)"
         H.li $ H.a H.! A.href "https://algwiki.moe/navy/index.html"     $ "Faction Category"
         H.li $ H.a H.! A.href "https://algwiki.moe/hull/index.html"     $ "Class Category"
         H.li $ H.a H.! A.href "https://algwiki.moe/rarity/index.html"   $ "Rarity Category"

         H.li H.! A.class_ "subheader"                                   $ "Tools"
         H.li $ H.a H.! A.href "https://sd.algwiki.moe/"                 $ "SD viewer"
         H.li $ H.a H.! A.href "https://l2d.algwiki.moe/"                $ "L2D viewer"
         H.li $ H.a H.! A.href "https://algwiki.moe/"                    $ "Player"

mkhtml :: String
       -> String
       -> String
       -> H.Html
       -> H.Html
       -> IO ()
mkhtml prefix name title y x
  = do css <- readFile "style.css"
       writeFile (prefix ++ name ++ ".html")
         $ renderHtml
         $ H.docTypeHtml
         $ do H.head
                $ do H.meta H.! A.httpEquiv "content-type" H.! A.content "text/html; charset=utf-8"
                     H.title (H.preEscapedToHtml $ title ++ " - /alg/ - Azur Lane General Wiki")
                     H.style H.! A.type_ "text/css" $ H.preEscapedToHtml css
                     y
              H.body
                $ do sidebar
                     H.div H.! A.id "sidebarEscape"
                       $ do x
                            H.footer
                              $ H.pre
                              $ H.preEscapedToHtml
                              $ "Azur Lane © is owned by Shanghai Manjuu, Xiamen Yongshi, Shanghai Yostar | All logos and trademarks are property of their respective owners.\n"
                              ++ "Special thanks to /alg/, English Koumakan Wiki, 碧蓝航线wiki, azurlane.wikiru.jp, and to all our contributors.\n"
                              ++ "/alg/ wiki | Copyright © 2021 alg-wiki | Contact at botebreeder@gmail.com | Source available at https://gitgud.io/alg-wiki/wikia\n"
                              ++ "This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.\n"
                              ++ "This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.\n"
                              ++ "You should have received a copy of the GNU Affero General Public License along with this program. If not, see http://www.gnu.org/licenses/."

loadJson :: String
         -> IO ((String, (String, String)), (String, String), (String, Aeson.Object))
loadJson x
  = do name <- return $ "Ships/" ++ x
       json <- (Aeson.eitherDecodeFileStrict' name :: IO (Either String Aeson.Object))
       case json of
         Left e -> error $ name ++ ": " ++ e
         Right json -> do nameen <- return $ json % "name_reference"
                          namecn <- return $ json % "cn_reference"
                          name   <- return $ json % "name"
                          rarity <- return $ json % "rarity"
                          return ((nameen, (namecn, rarity)), (nameen, name), (nameen, json))

lastN :: Int -> [a] -> [a]
lastN n xs = drop (length xs - n) xs

writeskins :: [(Int, String, Aeson.Object)]
           -> String
           -> Bool
           -> (Int -> String -> Aeson.Object -> H.Html)
           -> H.Html
writeskins [(i, i', x)] _ _ f = f i i' x
writeskins skins id e f
  = H.div H.! A.style "text-align: left;"
    $ do skins' <- return $ case id of
                              "lineView" -> skins
                              _ -> map (\(i, (_, i', x)) -> (i, i', x)) $ zip [0..] $ filter (\(_, i, _) -> not $ isInfixOf "_ex1" i) skins
         mapM_ (\(i, i', skin) -> do H.input H.! A.name (H.stringValue $ "skinSelectors-" ++ id) H.! A.autocomplete "off" H.! A.id (H.stringValue $ "skinSelector-" ++ id ++ "-" ++ show i) H.! A.type_ "radio" H.!? (i == 0, A.checked "") H.!? (e, A.onchange $ H.stringValue $ "skinChange(" ++ show i ++ ",\"" ++ i' ++ "\")")
                                     H.label H.! A.for (H.stringValue $ "skinSelector-" ++ id ++ "-" ++ show i)
                                       $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/herohrzicon/" ++ skin % "id" ++ ".png") H.! H.customAttribute "loading" "lazy" H.! A.style "height: 35px; width: 158px;margin: 0px 0px 0px 10px;") skins'

         mapM_ (\(i, i', skin) -> H.div H.! A.id (H.stringValue $ "skinContent-" ++ id ++ "-" ++ show i) H.! A.class_ "skinContent"
                                  $ f i i' skin) skins'

showship :: [(String, (String, String))]
         -> [(Int, String, Aeson.Object)]
         -> Aeson.Object
         -> H.Html
showship encn skins json
  = do H.tr
         $ do H.td
                $ H.table
                $ do H.tr $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "5" $ H.preEscapedToHtml $ (json % "name") ++ " (JP 🇯🇵: " ++ (json % "nameJP") ++ ", CN 🇹🇼: " ++ (json % "nameCN") ++ ")"
                     H.tr
                       $ do H.td H.! A.rowspan "3" $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/squareicon/" ++ (json % "cn_reference") ++ ".png")
                            H.th "Ship ID"
                            H.td $ do "No. "
                                      json %% "ID"
                            d "initialStar" "Star Rating"
                     H.tr
                       $ do H.th $ H.a H.! A.href "../hull/index.html" $ "Hull"
                            H.td $ do H.img H.! A.src (H.stringValue $ hull $ json % "hull") H.! A.style "height: auto; width: 35px"
                                      " "
                                      H.preEscapedToHtml $ capitalize $ json % "hull"
                            d "rarity" "Rarity"
                     H.tr
                       $ do H.th $ H.a H.! A.href "../navy/index.html" $ "Navy"
                            H.td $ do H.img H.! A.src (H.stringValue $ navy $ json % "navy")
                                      H.preEscapedToHtml $ capitalize $ json % "navy"
                            d "buildTime" "Build Time"
                     H.tr
                       $ do H.th H.! A.scope "row" $ "Acquisition"
                            H.td H.! A.colspan "4" $ H.preEscapedToHtml $ json % "acquisitionMethod"
                     H.tr
                       $ do H.th H.! A.scope "row" $ "Enhance Income"
                            H.td H.! A.colspan "4" $ showtable' json "3" "enhance" "height:auto;width:25px;"
                     H.tr
                       $ do H.th H.! A.scope "row" $ "Scrap Income"
                            H.td H.! A.colspan "4" $ showtable' json "3" "scrap" ""
              H.td
                $ H.table
                $ do H.tr $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "2" $ "Information"
                     H.tr $ do H.th H.! A.scope "row" $ "Release Date"
                               H.td $ H.table $ showtable json "1" "releaseDate"
                     H.tr $ do H.th H.! A.scope "row" $ "Voice actress"
                               H.td $ H.preEscapedToHtml $ json % "voiceActress"
                     H.tr $ H.th H.! A.class_ "subtitle" H.! A.scope "col" H.! A.colspan "2" $ "Illustrator"
                     showtable json "1" "artist"

       H.tr
         $ H.td H.! A.colspan "2"
         $ H.table H.! A.class_ "full"
         $ do H.tr
                $ H.th H.! A.class_ "title" H.! A.scope "col" $ "Skins"
              H.tr
                $ do H.td
                     $ writeskins skins "skinView" True
                       $ \_ -> \i -> \skin -> do H.table
                                                   $ do H.tr
                                                          $ do H.th H.! A.style "min-width: 50%" H.! A.class_ "subtitle" $ skin %% "name"
                                                               H.th H.! A.class_ "subtitle" $ "Description"
                                                        H.tr
                                                          $ do H.td H.! A.rowspan "5" $ H.img H.! A.id (H.stringValue $ "painting-" ++ i) H.! A.src (H.stringValue $ "https://algwiki.moe/assets/painting/" ++ skin % "id" ++ ".png") H.! A.style "max-width: 100%;"
                                                               H.td $ skin %% "description"
                                                        H.tr
                                                          $ H.td H.! A.id (H.stringValue $ "containerSD-" ++ i)
                                                          $ do H.select H.! A.id (H.stringValue $ "selectAnimation-" ++ i) $ ""
                                                               H.div H.! A.id (H.stringValue $ "canvasSD-" ++ i) $ ""
                                                        H.tr $ H.th H.! A.class_ "subtitle" $ "Expressions"
                                                        H.tr $ H.td H.! A.id (H.stringValue $ "shipSkinExpressions-" ++ i) $ ""

       H.tr
         $ do H.td
                $ H.table
                $ do H.tr $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "2" $ "Parameters"
                     mapM_ (\k -> let par = aobj $ json ! "parameters"
                                  in
                                    H.tr
                                    $ do H.td H.! A.style "text-align: left; padding-left:5px;"
                                           $ do H.img H.! A.style "width:25px;" H.! A.src (H.stringValue $ funny k)
                                                " "
                                                H.preEscapedToHtml $ capitalize k
                                         H.td H.! A.style "text-align: left; padding-left:5px;" $ par %% k)
                       $ ["hp",
                          "antiAir",
                          "evasion",
                          "aviation",
                          "torpedo",
                          "firepower"]
                     H.tr
                       $ H.th H.! A.class_ "subtitle" H.! A.scope "col" H.! A.colspan "2"
                       $ "Stats"
                     stats <- return
                              $ map (\(k, Aeson.Object v) -> (unpack k, v))
                              $ toList
                              $ aobj
                              $ json ! "stats"
                     H.tr
                       $ H.td H.! A.colspan "2"
                       $ do mapM_ (\(i, _) -> do H.input H.! A.name "skinSelectors-stat" H.! A.autocomplete "off" H.! A.id (H.stringValue $ "statSelector-" ++ i)  H.! A.type_ "radio" H.!? (i == "base", A.checked "")
                                                 H.label H.! A.for (H.stringValue $ "statSelector-" ++ i) H.! A.style "margin-left: 5px;border-style: solid;"
                                                   $ H.preEscapedToHtml $ case i of
                                                                  "base" -> "Base"
                                                                  "100retrofit" -> "100 Retrofit"
                                                                  "120retrofit" -> "120 Retrofit"
                                                                  x -> x) stats
                            mapM_ (\(i, v) -> H.div H.! A.id (H.stringValue $ "statContent-" ++ i) H.! A.class_ "skinContent"
                                              $ H.table
                                              $ do mapM_ (\(k1, k2)
                                                          -> H.tr
                                                             $ do H.th H.! A.style "text-align: left; padding-left:5px;"
                                                                    $ do H.img H.! A.style "width:25px;" H.! A.src (H.stringValue $ funny k1)
                                                                         " "
                                                                         H.preEscapedToHtml $ capitalize k1
                                                                  H.td H.! A.style "width:15%;text-align:center" $ v %% k1
                                                                  H.th H.! A.style "text-align: left; padding-left:5px;"
                                                                    $ do H.img H.! A.style "width:25px;" H.! A.src (H.stringValue $ funny k2)
                                                                         " "
                                                                         H.preEscapedToHtml $ capitalize k2
                                                                  H.td H.! A.style "width:15%;text-align:center" $ v %% k2)
                                                     $ [("hp", "reload"),
                                                        ("firepower", "torpedo"),
                                                        ("evasion", "antiAir"),
                                                        ("aviation", "cost"),
                                                        ("asw", "luck")]
                                                   H.tr
                                                     $ do H.th H.! A.style "text-align: left; padding-left:5px;"
                                                            $ do H.img H.! A.style "width:25px;" H.! A.src (H.stringValue $ funny "hit")
                                                                 " Hit"
                                                          H.td H.! A.style "width:15%;text-align:center" $ v %% "luck"
                                                          H.th H.! A.style "text-align: left; padding-left:5px;"
                                                            $ "Speed"
                                                          H.td H.! A.style "width:15%;text-align:center" $ v %% "speed"
                                                   H.tr
                                                     $ do H.th H.! A.style "text-align: left; padding-left:5px;"
                                                            $ do H.img H.! A.style "width:25px;" H.! A.src (H.stringValue $ funny "armor")
                                                                 " Armor"
                                                          H.td H.! A.style "width:15%;text-align:center" H.! A.colspan "3" $ v %% "armor") stats

              H.td
                $ H.table
                $ do H.tr $ H.th H.! A.class_ "title"  H.! A.scope "col" H.! A.colspan "5" $ "Limit Break"
                     mapM_ (\(k, v) -> H.tr
                                       $ do H.td H.! A.style "text-align: left; padding-left:5px;"
                                              $ H.preEscapedToHtml $ case lastN 2 k of
                                                             ('r':x) -> "Tier " ++ x
                                                             ('0':x) -> "Level " ++ x
                                                             x       -> "Level " ++ x
                                            H.td H.! A.colspan "4" H.! A.style "text-align: left; padding-left:5px;" $ H.preEscapedToHtml $ ashow v)
                       $ sortOn (\(k, _) -> k)
                       $ map (\(k, v) -> (case lastN 2 $ unpack k of
                                            ('r':x) -> "r" ++ x
                                            ('l':x) -> "0" ++ x
                                            x       -> x, v))
                       $ toList
                       $ aobj
                       $ case HM.lookup "limitBreak" json of
                           Nothing -> json ! "strengthenLevel"
                           Just x -> x
                     H.tr $ H.th H.! A.class_ "subtitle" H.! A.scope "col" H.! A.colspan "5" $ "Equipments"
                     H.tr
                       $ mapM_ (H.th H.! A.class_ "subtitle" H.! A.scope "col")
                       $ ["Slot",
                          "Equipment Type",
                          "Efficiency (LB 0/1/2/3)",
                          "Quantity (LB 0/1/2/3)",
                          "Preload (LB 0/1/2/3)"]
                     mapM_ (\(k, Aeson.Object v) -> H.tr
                                                    $ do H.td $ H.preEscapedToHtml k
                                                         mapM_ (\x -> H.td $ v %% x) ["type", "efficiency", "amount", "preload"]) $ sortOn (\(k, _) -> k) $ toList $ aobj $ json ! "equipmentLoadout"
                     H.tr
                       $ do H.th H.! A.class_ "subtitle" H.! A.scope "col" H.! A.colspan "5" $ "Default Equipments"
                            mapM_ (\(k, Aeson.Object v) -> H.tr
                                                           $ do H.td $ H.preEscapedToHtml k
                                                                H.td H.! A.colspan "4" $ v %% "name") $ sortOn (\(k, _) -> k) $ toList $ aobj $ json ! "defaultEquipment"

       case HM.lookup "fleet_tech" json of
         Just (Aeson.Object ft)
           -> H.tr
              $ H.td H.! A.colspan "2"
              $ H.table
              $ do H.tr
                     $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "3" H.! A.style "width:40%;" $ "Fleet Tech"
                   H.tr
                     $ do H.th H.! A.class_ "subtitle"
                            $ do "T"
                                 ft %% "t_level"
                                 " "
                                 json %% "hull"
                                 ": "
                                 H.preEscapedToHtml $ ashow $ HM.lookupDefault "##undefined##" "class" json
                          H.th H.! A.class_ "subtitle" H.! A.colspan "2" H.! A.style "width:60%;" $ "Tech Points and Bonus"
                   H.tr
                     $ do H.td
                            $ mapM_ (\x -> let (cn, rarity) = fromJust $ lookup x encn
                                           in
                                             H.a H.! A.href (H.stringValue $ x ++ ".html")
                                             $ H.img H.! A.style (H.stringValue $ "width:64px;height:64px;margin:10px 0px 0px 10px;box-sizing: border-box;border-radius: 8px;border: 2px solid white;background: url(\"https://algwiki.moe/Images/"
                                                                  ++ case rarity of
                                                                       "Ultra Rare" -> "bg5"
                                                                       "Decisive" -> "bg5"
                                                                       "Priority" -> "bg4"
                                                                       "Super Rare" -> "bg4"
                                                                       "Elite" -> "bg3"
                                                                       "Rare" -> "bg2"
                                                                       "Common" -> "bg1"
                                                                       "" -> ""
                                                                  ++ ".png\");") H.! A.src (H.stringValue $ "https://algwiki.moe/assets/squareicon/" ++ cn ++ ".png")) $ endBy "," $ ft % "ships"
                          H.td
                            $ writeskins skins "techView" False
                            $ \_ -> \_ -> \skin -> H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/shipmodels/" ++ skin % "id" ++ ".png")
                          H.td
                            $ H.table
                            $ do H.tr
                                   $ do H.th "Unlock"
                                        H.td $ ft %% "pt_get"
                                        H.td
                                          $ do mapM_ (\x -> do H.img H.! A.style "width: 35px;" H.! A.src (H.stringValue $ hull x)
                                                               " ") $ endBy "," $ ft % "add_get_shiptype"
                                               H.img H.! A.style "width: 25px;" H.! A.src (H.stringValue $ funny $ map toLower $ ft % "add_get_attr")
                                               " +"
                                               ft %% "add_get_value"
                                 H.tr
                                   $ do H.th "Max LimitBreak"
                                        H.td $ ft %% "pt_upgrade"
                                        H.td ""
                                 H.tr
                                   $ do H.th "Lv.120"
                                        H.td $ ft %% "pt_level"
                                        H.td
                                          $ do mapM_ (\x -> do H.img H.! A.style "width: 35px;" H.! A.src (H.stringValue $ hull x)
                                                               " ") $ endBy "," $ ft % "add_level_shiptype"
                                               H.img H.! A.style "width: 25px;" H.! A.src (H.stringValue $ funny $ map toLower $ ft % "add_level_attr")
                                               " +"
                                               ft %% "add_level_value"
         Nothing -> return ()

       H.tr
         $ H.td H.! A.colspan "2"
         $ H.table
         $ do H.tr $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "4" $ "Skillset"
              H.tr $ mapM_ (H.th H.! A.scope "col" H.! A.class_ "subtitle") ["Icon", "Name", "Description", "Requirements"]
              mapM_ (\(k, Aeson.Object v) -> H.tr
                                             $ do H.td $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/skillicon_new/" ++ (v % "icon") ++ ".png")
                                                  mapM_ (\x -> H.td $ v %% x) ["name", "description", "requirement"]) $ reverse $ toList $ aobj $ json ! "skill"

       H.tr
         $ H.td H.! A.colspan "2"
         $ H.table
         $ do H.tr
                $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "4"
                $ "Construction"
              H.tr
                $ do H.td H.! A.class_ "subtitle" H.! A.style "width:20%;" $ json %% "buildTime"
                     mapM_ (H.th H.! A.class_ "subtitle" H.! A.style "width:20%;" H.! A.scope "col") ["JP", "CN", "EN"]
              mapM_ (\(k, Aeson.Object v) -> H.tr
                                             $ do H.th H.! A.scope "row" H.! A.class_ "subtitle"
                                                    $ H.preEscapedToHtml
                                                    $ capitalize
                                                    $ unpack k
                                                  mapM_ (\x -> H.td $ v %% x) ["JP", "CN", "EN"])
                $ reverse
                $ toList
                $ aobj
                $ json ! "build"

       H.tr
         $ H.td H.! A.colspan "2"
         $ H.table
         $ do linesSet <- return
                          $ map (\(Aeson.Object x) -> (capitalize $ x % "skin_id", x))
                          $ elems
                          $ aobj
                          $ (aobj $ json ! "lines") ! "skin"
              H.tr
                $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "5"
                $ "Dialogue"
              H.tr
                $ H.td
                $ writeskins skins "lineView" False
                $ \i -> \n -> \skin -> case lookup (capitalize n) linesSet of
                                         Just lineSet
                                           -> do lines <- return
                                                          $ map aobj
                                                          $ elems
                                                          $ aobj
                                                          $ lineSet ! "dialogue"
                                                 H.table
                                                   $ do H.tr
                                                          $ mapM_ (\(x, y) -> H.th H.! A.class_ "subtitle" H.! A.scope "col" H.! A.style (H.stringValue $ "width:" ++ show y ++ "%;") $ x)
                                                          $ [("Event", 14),
                                                             ("", 3),
                                                             ("West Taiwanese Server", 27),
                                                             ("Japanese Server", 27),
                                                             ("English Server", 27)]
                                                        mapM_ (\k
                                                               -> case find (\x -> x % "event" == k) lines of
                                                                    Just x
                                                                      -> H.tr
                                                                         $ do H.th H.! A.scope "row" $ H.preEscapedToHtml k
                                                                              H.td
                                                                                $ case x % "media" of
                                                                                    "" -> ""
                                                                                    s  -> H.audio H.! A.preload "none" H.! A.src (H.stringValue
                                                                                                                                   $ "https://algwiki.moe/assets/cue/cv-"
                                                                                                                                   ++ init (case json % "internal_id" of
                                                                                                                                              "" -> "0"
                                                                                                                                              x -> x)
                                                                                                                                   ++ (if any (\x -> x `isPrefixOf` s) ["hp", "lose", "mvp", "skill", "warcry", "link"] then "-battle" else "")
                                                                                                                                   ++ "/acb/awb/"
                                                                                                                                   ++ s
                                                                                                                                   ++ ".ogg") H.! A.controls "" $ ""
                                                                              H.td $ x %% "chinese"
                                                                              H.td $ x %% "japanese"
                                                                              H.td $ x %% "english"
                                                                    Nothing -> return ())
                                                          $ ["Ship Description",
                                                             "Biography",
                                                             "Acquisition",
                                                             "Login",
                                                             "Details",
                                                             "Main 1",
                                                             "Main 2",
                                                             "Main 3",
                                                             "Touch",
                                                             "Touch (Special)",
                                                             "Mission",
                                                             "Mission Complete",
                                                             "Mail",
                                                             "Return to Port",
                                                             "Commission Complete",
                                                             "Enhancement",
                                                             "Flagship",
                                                             "Victory",
                                                             "Defeat",
                                                             "Skill",
                                                             "Low HP",
                                                             "Affinity (Upset)",
                                                             "Affinity (Stranger)",
                                                             "Affinity (Friendly)",
                                                             "Affinity (Like)",
                                                             "Affinity (Love)",
                                                             "Pledge"]
                                         Nothing
                                           -> "Missing lines!!"

  where d = displayRow json

displayRow :: Aeson.Object
           -> String
           -> H.Html
           -> H.Html
displayRow json field text
  = do H.th text
       H.td $ json %% field

order = ["common", "research", "META", "collab", "unreleased"]

indexFood :: String
          -> [(String, Aeson.Object)]
          -> H.Html
indexFood lvl ships
  = H.div H.! A.class_ "container"
    $ mapM_ (\(id, json) -> H.div H.! A.class_ "ship"
                            $ do a <- return $ H.a H.! A.href (H.stringValue $ lvl ++ "ships/" ++ json % "link" ++ ".html")
                                 mapM_ (\(x, f) -> H.a H.! A.href (H.stringValue $ lvl ++ x ++ "/" ++ json % x ++ ".html")
                                                   $ H.img H.! A.src (H.stringValue $ f $ json % x) H.! A.title (H.stringValue $ json % x) H.! A.class_ (H.stringValue x))
                                   $ [("hull", hull),
                                      ("navy", navy)]
                                 H.preEscapedToHtml id
                                 H.div H.! A.style (H.stringValue $ "background: " ++ decideColor (json % "rarity") ++ ";")
                                   $ a $ H.img H.! A.src (H.stringValue $ json % "icon") H.! H.customAttribute "loading" "lazy" H.! A.style "height:144px;width: 108px"
                                 H.div H.! A.class_ "name" $ a $ json %% "name") ships

makeMainIndex :: String
              -> String
              -> [[(String, Aeson.Object)]]
              -> IO ()
makeMainIndex file title ships
  = do mkhtml "out/" file title (return ())
         $ do H.nav
                $ do H.a H.! A.href "." $ "Home"
                     " > "
                     H.preEscapedToHtml title
              mapM_ (\x -> H.details H.! A.open ""
                           $ do H.summary $ H.h2 H.! A.style "display: inline;" $ H.preEscapedToHtml $ capitalize $ (snd $ head x) % "shipType"
                                indexFood "" x) ships

g :: [(String, Aeson.Object)]
  -> [[(String, Aeson.Object)]]
g x
  = map (sortOn (\(id, _) -> case readEither id :: Either String Int of
                               Left  x -> case readEither (tail id) :: Either String Int of
                                            Left x -> 0
                                            Right x -> x
                               Right x -> x))
    $ groupBy (\(_, a) -> \(_, b) -> a % "shipType" == b % "shipType")
    $ sortBy (comparing $ \a -> elemIndex ((snd a) % "shipType") order) x

makeIndex :: String
          -> (String -> String)
          -> [(String, Aeson.Object)]
          -> IO ()
makeIndex category f ships
  = do createDirectory $ "out/" ++ category
       subcats <- mapM (\x -> do name <- return $ case (snd $ head $ head x) % category of
                                                    "" -> "unknown"
                                                    x -> x
                                 mkhtml ("out/" ++ category ++ "/") name (capitalize name) (return ())
                                   $ do H.nav
                                          $ do H.a H.! A.href ".." $ "Home"
                                               " > "
                                               H.a H.! A.href "./index.html" $ H.preEscapedToHtml $ capitalize category
                                               " > "
                                               H.preEscapedToHtml $ capitalize name
                                        mapM_ (\x -> H.details H.! A.open ""
                                                     $ do H.summary $ H.h2 H.! A.style "display: inline;" $ H.preEscapedToHtml $ capitalize $ (snd $ head x) % "shipType"
                                                          indexFood "../" x) x
                                 return name) groupedShips
       mkhtml ("out/" ++ category ++ "/") "index" (capitalize category) (return ())
         $ do H.nav
                $ do H.a H.! A.href ".." $ "Home"
                     " > "
                     H.preEscapedToHtml $ capitalize category
              H.ol
                $ mapM_ (\x -> H.li
                               $ H.a H.! A.href (H.stringValue $ x ++ ".html")
                               $ do H.img H.! A.src (H.stringValue $ f x) H.! A.style "max-width: 64px; max-height: 64px;"
                                    H.preEscapedToHtml x) subcats
  where
    groupedShips :: [[[(String, Aeson.Object)]]]
    groupedShips
      = map g
        $ groupBy (\(_, a) -> \(_, b) -> a % category == b % category)
        $ sortBy (comparing $ \a -> (snd a) % category) ships

showtable json i k
  = mapM_ (\(k, v) -> H.tr
                      $ do H.td H.! A.colspan i $ H.preEscapedToHtml $ capitalize $ unpack k
                           H.td $ H.preEscapedToHtml $ ashow v) $ reverse $ toList $ aobj $ json ! k

showtable'  json i k style
  = H.table
    $ mapM_ (\(k, v) -> H.tr
                        $ do H.td H.! A.style "text-align: left; width: 70%;" H.! A.colspan i
                               $ do H.img H.! A.src (H.stringValue $ "https://algwiki.moe/Images/" ++ unpack k ++ "_icon.png") H.! A.style style
                                    " "
                                    H.preEscapedToHtml $ capitalize $ unpack k
                             H.td H.! A.style "text-align: left;padding-left:5px;" $ H.preEscapedToHtml $ ashow v) $ reverse $ toList $ aobj $ json ! k

capitalize "hp" = "HP"
capitalize "antiAir" = "Anti-air"
capitalize "asw" = "ASW"
capitalize (x:xs) = (toUpper x) : xs
capitalize [] = []

cute "anti-air" = "anti_air"
cute "antiAir" = "anti_air"
cute "hp" = "health"
cute x = x

funny x = "https://algwiki.moe/Images/" ++ cute x ++ "_icon.png"

decideColor "Ultra Rare" = "linear-gradient(to right, rgb(108, 174, 108), rgb(95, 176, 190), rgb(125, 132, 192), rgb(180, 84, 128))"
decideColor "Decisive"   = "linear-gradient(to right, rgb(108, 174, 108), rgb(95, 176, 190), rgb(125, 132, 192), rgb(180, 84, 128))"
decideColor "Priority"   = "rgb(190, 185, 136)"
decideColor "Super Rare" = "rgb(190, 185, 136)"
decideColor "Elite"      = "rgb(176, 128, 176)"
decideColor "Rare"       = "rgb(140, 179, 184)"
decideColor "Common"     = "rgb(115, 115, 115)"
decideColor ""           = "#24252d"

main :: IO ()
main
  = do catchIOError (removeDirectoryRecursive "out") $ const $ return ()
       createDirectory "out"
       createDirectory "out/ships"
       dir <- listDirectory "Ships"
       (encn, enen, ships) <- (mapM loadJson $ sort dir) >>= return . unzip3
       dumbjs <- readFile "dumbjs.js"
       mapM_ (\(name, json) -> let skins = map (\(i, (k, Aeson.Object v)) -> (i, v % "id", v))
                                           $ zip [0..]
                                           $ sortOn (\(k, _) -> k)
                                           $ toList
                                           $ aobj
                                           $ json ! "skin"
                               in
                                 mkhtml "out/ships/" name (json % "name") (do H.style H.! A.type_ "text/css" $ H.preEscapedToHtml $ ".title {background: " ++ decideColor (json % "rarity") ++ ";}"
                                                                              mapM_ (\x -> H.script H.! A.src (H.stringValue x) $ "")
                                                                                $ ["https://algwiki.moe/js/pixi.min-4.7.1.js",
                                                                                   "https://algwiki.moe/js/live2dcubismcore.min.js",
                                                                                   "https://algwiki.moe/js/live2dcubismframework.js",
                                                                                   "https://algwiki.moe/js/live2dcubismpixi.js",
                                                                                   "https://algwiki.moe/js/pixi-spine.js",
                                                                                   "https://algwiki.moe/js/SkeletonBinary.js"])
                               $ do H.nav
                                      $ do H.a H.! A.href "/" $ "Home"
                                           " > "
                                           H.a H.! A.href "../shiplist.html" $ "Shiplist"
                                           " > "
                                           json %% "name"
                                    H.main $ H.table $ showship encn skins json
                                    H.script $ H.preEscapedToHtml $ "skins = [" ++ (skins >>= (\(_, _, x) -> "[\"" ++ x % "id" ++ "\"," ++ ((keys $ aobj $ x ! "expression") >>= \x -> "\"" ++ unpack x ++ "\",") ++ "],")) ++ "];" ++ dumbjs) ships

       shiplist'' <- (Aeson.eitherDecodeFileStrict' "json/shiplist.json" :: IO (Either String Aeson.Object))
       shiplist' <- case shiplist'' of
                      Left  e    -> error $ "shiplist error: " ++ e
                      Right json -> return $ map (\(id, json) -> (unpack id, aobj json)) $ toList json
       shiplist <- return
                   $ g shiplist'

       makeIndex "rarity" rarity shiplist'
       makeIndex "hull"   hull   shiplist'
       makeIndex "navy"   navy   shiplist'
       makeMainIndex "shiplist" "Shiplist (By ID)" shiplist
       makeMainIndex "shiplist_alpha" "Shiplist (Alphabetic)"
         $ map (sortOn (\(_, json) -> json % "name")) $ shiplist
