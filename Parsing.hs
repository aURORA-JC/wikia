module Parsing where

import Text.Parsec (runParser)

import Data
import JSON
import Lua

import Data.List (isSuffixOf)

import Control.Exception (catch, IOException)

langs = ["cn", "jp", "en"]

readJsonLang :: String -> IO Expr
readJsonLang x
  = (parse $ "json/" ++ x ++ ".json") `catch` handler
  where handler :: IOException -> IO Expr
        handler _ = parse $ "lua/" ++ x ++ ".lua"

readJsonLangs :: String -> IO [Expr]
readJsonLangs x
  = mapM (\lang -> readJsonLang $ x ++ "." ++ lang) langs

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
