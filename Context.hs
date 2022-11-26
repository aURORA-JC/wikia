module Context where

import Data

data Context
  = Context
  {
    ctx_luaskin :: [Expr],
    ctx_luaskinextra :: [Expr],
    ctx_namecode :: [[(Int, (String, String))]],
    ctx_encn :: [(String, (String, String))],
    ctx_skins :: [(Int, String, Expr)],
    ctx_json :: Expr,
    ctx_ships :: [Expr],
    ctx_ship_data_template :: Expr,
    ctx_skill_data_template :: Expr,
    ctx_ship_data_blueprint :: Expr,
    ctx_ship_strengthen_blueprint :: Expr,
    ctx_ship_data_breakout :: Expr,
    ctx_ship_strengthen_meta :: Expr,
    ctx_ship_meta_breakout :: Expr,
    ctx_spweapon_data_statistics :: Expr,
    ctx_ship_skin_words_add :: [Expr],
    ctx_gametips :: Expr
  }
