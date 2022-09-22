{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Showskills where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Utils
import Data
import System.IO.Unsafe

import Context
import Debug.Trace

subst x with
  = H.preEscapedToHtml
    $ subst' x with

subst' "" _
  = ""

subst' ('$':x:xs) with
  = let with' = byindex with ((read [x] :: Int) - 1) in
      (ashow $ byindex (byindex with' 0) 0) ++ " (" ++ (ashow $ byindex (byindex with' $ (length $ toList with') - 1) 0) ++ ") " ++ subst' xs with


subst' (x:xs) with
  = x:(subst' xs with)

showskills context
  = H.table
    $ do json <- return $ ctx_json context
         ship_data_template <- return $ ctx_ship_data_template context
         skill_data_template <- return $ ctx_skill_data_template context
         ship_data_blueprint <- return $ ctx_ship_data_blueprint context
         ship_strengthen_blueprint <- return $ ctx_ship_strengthen_blueprint context
         spweapon_data_statistics <- return $ ctx_spweapon_data_statistics context

         id <- return
               $ init
               $ case ashow
                      $ json ! "internal_id" of
                   "" -> "a"
                   x  -> x

         buff_list <- return
                      $ (map (\x -> case lookups ship_data_template (id ++ [x]) of
                                      Just x | x % "star" == x % "star_max" -> [map ashow $ elems $ x ! "buff_list_display"]
                                      _ -> []) ['1'..'9'] >>= concat)
                      ++ ((elems spweapon_data_statistics)
                           >>= (\x -> case lookups x "unique" of
                                        Just y | ashow y == id -> case lookups x "skill_upgrade" of
                                                                    Just x -> case elems x of
                                                                                [v]    -> [ashow ((elems v) !! 1)]
                                                                                _      -> []
                                                                    _      -> []
                                        _                      -> []))

         -- for each fate_strengthen and strengthen_effect
         -- check in ship_strengthen_blueprint for change skill
         -- then print the skill from skill_data_template
         skills <- return
                   $ map (\x -> skill_data_template ! x)
                   $ buff_list
                   ++ (case lookups ship_data_blueprint id of
                         Just lst
                           -> let fate = elems $ lst ! "fate_strengthen"
                                  strengthen = elems $ lst ! "strengthen_effect"
                              in
                                concat
                                $ map (\(x :: Expr)
                                       -> let blueprint = ship_strengthen_blueprint ! (asstr $ asval x)
                                          in
                                            case blueprint ! "change_skill" of
                                              Obj _ [_, (_, b)] -> [asstr $ asval b]
                                              _                 -> [])
                                $ strengthen ++ fate
                         Nothing -> [])

         H.tr $ H.th H.! A.class_ "title" H.! A.scope "col" H.! A.colspan "4" $ "Skillset"
         H.tr $ mapM_ (H.th H.! A.scope "col" H.! A.class_ "subtitle") ["Icon", "Name", "Description", "Requirements"]
         mapM_ (\x -> do H.tr
                           $ do H.td $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/skillicon_new/" ++ (case x % "id" of
                                                                                                                          ['2', _, '3', _, _] -> "20300"
                                                                                                                          ['2', _, '2', _, _] -> "20200"
                                                                                                                          ['2', _, '1', _, _] -> "20100"
                                                                                                                          ['2', _, '0', _, _] -> "20000"
                                                                                                                          ['1', '9', x, y, _] -> ['1', '9', x, y, '0']
                                                                                                                          ['2', '9', x, y, _] -> ['2', '9', x, y, '0']
                                                                                                                          x -> x) ++ ".png")
                                H.td $ x %% "name"
                                H.td $ subst (x % "desc") (x ! "desc_add")
                                H.td $ case filter (\y -> y % "icon" == x % "id") $ elems $ json ! "skill" of
                                         [x] -> x %% "requirement"
                                         _   -> "???") skills

{-
OLD:
         mapM_ (\(k, v) -> H.tr
                                $ do H.td $ H.img H.! A.src (H.stringValue $ "https://algwiki.moe/assets/skillicon_new/" ++ (v % "icon") ++ ".png")
                                     mapM_ (\x -> H.td $ v %% x) ["name", "description", "requirement"]) $ sortOn (\(k, v) -> read (k) :: Int) $ toList $ json ! "skill"
-}
