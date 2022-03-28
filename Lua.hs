module Lua where

import Text.Parsec ((<|>), (<?>), satisfy, char, between, many, many1, Parsec, space, sepEndBy, sepEndBy1, eof, getPosition)
import qualified Text.Parsec
import Data.Char
import Control.Monad
import Data.Functor.Identity

import Data

eat
  = many space

value
  = do pos <- getPosition
       (do x <- (number >>= return . Num)
                <|> (string >>= return . Str)
           return
             $ Val pos x)
        <|> (block >>= return . Obj pos) <?> "value"

sep
  = do char ','
       eat

iblock
  = ((do char '['
         eat
         x <- number
         eat
         char ']'
         eat
         char '='
         eat
         v <- value
         eat
         return (show x, v))
      <|> (do v <- value
              eat
              return ("", v))) <?> "iblock"

block
  = (between (char '{') (do char '}'
                            eat)
     $ ((do char ','
            eat
            iblock `sepEndBy1` sep)
        <|> iblock `sepEndBy` sep)) <?> "block"

lua
  = do bom
       eat
       Text.Parsec.string "pg = pg or {}\npg." <|> return ""
       eat
       x <- value
       eat
       eof
       return x
