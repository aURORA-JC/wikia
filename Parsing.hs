module Parsing where

import Text.Parsec (runParser)

import Data
import JSON
import Lua

import Data.List (isSuffixOf)

langs = ["cn", "jp", "en"]

readJsonLangs :: String -> IO [Expr]
readJsonLangs x
  = mapM (\lang -> do file <- return $ "json/" ++ x ++ "." ++ lang ++ ".json"
                      parse file) langs

parse :: String -> IO Expr
parse path
  = do content <- readFile path
       case runParser (if ".bote" `isSuffixOf` path || ".json" `isSuffixOf` path then
                          json
                       else if ".lua" `isSuffixOf` path then
                          lua
                       else
                          error "Unknown filetype") () path content of
         Left e -> error $ "Parsing error at file " ++ path ++ ": " ++ show e
         Right x -> return x
