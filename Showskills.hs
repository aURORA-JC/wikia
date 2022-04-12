{-# LANGUAGE OverloadedStrings #-}

module Showskills where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Utils
import Data

showskills json ship_data_template skill_data_template
  = H.table
    $ do id <- return
               $ init
               $ case ashow
                      $ json ! "internal_id" of
                   "" -> "a"
                   x  -> x
         buff_list <- return
                      $ map (\x -> case lookups ship_data_template (id ++ [x]) of
                                     Just x | x % "star" == x % "star_max" -> [map ashow $ elems $ x ! "buff_list_display"]
                                     _ -> []) ['1'..'9'] >>= concat
         skills <- return
                   $ map (\x -> skill_data_template ! x)
                   $ buff_list
         H.tr $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "4" $ "Skillset"
         H.tr $ mapM_ (H.th H.! A.scope "col" H.! A.class_ "subtitle") ["Icon", "Name", "Description", "Requirements"]
         mapM_ (\x -> H.tr
                      $ do H.td $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/skillicon_new/" ++ (x % "id") ++ ".png")
                           H.td $ x %% "name"
                           H.td $ x %% "desc"
                           H.td $ case filter (\y -> y % "icon" == x % "id") $ elems $ json ! "skill" of
                                    [x] -> x %% "requirement"
                                    _   -> "???") skills
{-
OLD:
         mapM_ (\(k, v) -> H.tr
                                $ do H.td $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/skillicon_new/" ++ (v % "icon") ++ ".png")
                                     mapM_ (\x -> H.td $ v %% x) ["name", "description", "requirement"]) $ sortOn (\(k, v) -> read (k) :: Int) $ toList $ json ! "skill"
-}
