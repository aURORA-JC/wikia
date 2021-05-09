module Extra where

import Text.Parsec ((<|>), (<?>), satisfy, char, between, many, many1, string, ParsecT, runParserT)
import Data.Char
import Control.Monad
import Data.Functor.Identity

data Val
  = Num Int
  | Str String
  | Block [(Maybe (Either String Int), Val)]
  deriving Show

readnum
  = (do ((char '-' >> return ()) <|> return ())
        x <- many1 $ satisfy isDigit
        return (read x :: Int)) <?> "Number"

identifier
  = (do x <- satisfy $ \x -> isAlpha x || x == '_'
        xs <- many (satisfy $ \x -> isAlphaNum x || x == '_' || x == '-')
        return (x:xs)) <?> "Identifier"

str :: ParsecT String () Identity String
str
  = (between (char '"') (char '"')
     $ many ((do char '\\'
                 (do char '"'
                     return '\"')
                   <|> (do char '\\'
                           return '\\')
                   <|> (do char 'n'
                           return '\n'))
              <|> satisfy ((/=) '"'))) <?> "String"


tok :: ParsecT String () Identity x -> ParsecT String () Identity x
tok x
  = (do many $ satisfy isSpace
        x) <?> "Token"

val
  = ((readnum >>= return . Num)
     <|> (str >>= return . Str)
     <|> (block >>= return . Block)) <?> "Value"

element
  = ((do id <- ((identifier >>= return . Left)
                <|> (between (char '[') (char ']') readnum >>= return . Right))
         tok $ char '='
         v <- tok $ val
         return (Just id, v))
     <|> (do v <- val
             return (Nothing, v))) <?> "Element"

iblock
  = (many
     $ do e <- tok
               $ element
          tok ((char ',' >> return ()) <|> return ())
          return e) <?> "IBlock"

block
  = (between (char '{') (tok
                         $ char '}')
     $ iblock) <?> "Block"

skipbad
  = string "pg = pg or {}\npg." <|> return ""

start :: String -> String -> [(Maybe (Either String Int), Val)]
start file s
  = case runIdentity $ runParserT (skipbad >> iblock) () file s of
      Left x -> error $ "\n" ++ show x ++ "\n"
      Right x -> x

lookupi :: Val -> Int -> Maybe Val
lookupi (Block ((Just (Right j), v):xs)) i | i == j = Just v
lookupi (Block (_:xs)) i = lookupi (Block xs) i
lookupi _ _ = Nothing

lookups :: Val -> String -> Maybe Val
lookups (Block ((Just (Left j), v):xs)) i | i == j = Just v
lookups (Block (_:xs)) i = lookups (Block xs) i
lookups _ _ = Nothing

asnum :: Val -> Int
asnum (Num x) = x

asstr :: Val -> String
asstr (Str s) = s
