#!/bin/env runhaskell

import Text.Parsec ((<|>), satisfy, char, between, many, many1, ParsecT, runParserT)
import Data.Char
import Control.Monad
import Data.Functor.Identity

data Val
  = Num Int
  | Str String
  | Block [(Maybe (Either String Int), Val)]
  deriving Show

readnum
  = do ((char '-' >> return ()) <|> return ())
       x <- many1 $ satisfy isDigit
       return (read x :: Int)

identifier
  = do x <- satisfy $ \x -> isAlpha x || x == '_'
       xs <- many (satisfy $ \x -> isAlphaNum x || x == '_' || x == '-')
       return (x:xs)

string :: ParsecT String () Identity String
string
  = between (char '"') (char '"')
    $ many ((do char '\\'
                (do char '"'
                    return '\"')
                  <|> (do char '\\'
                          return '\\')
                  <|> (do char 'n'
                          return '\n'))
             <|> satisfy ((/=) '"'))


tok :: ParsecT String () Identity x -> ParsecT String () Identity x
tok x
  = do many $ satisfy isSpace
       x

val
  = (readnum >>= return . Num)
    <|> (string >>= return . Str)
    <|> (block >>= return . Block)

element
  = (do id <- ((identifier >>= return . Left)
               <|> (between (char '[') (char ']') readnum >>= return . Right))
        tok $ char '='
        v <- tok $ val
        return (Just id, v))
    <|> (do v <- val
            return (Nothing, v))

iblock
  = many
    $ do e <- tok
              $ element
         tok ((char ',' >> return ()) <|> return ())
         return e

block
  = between (char '{') (tok
                        $ char '}')
    $ iblock

start :: String -> [(Maybe (Either String Int), Val)]
start s
  = case runIdentity $ runParserT iblock () "" s of
      Left x -> error $ show x
      Right x -> x

main = getContents >>= putStrLn . show . start
