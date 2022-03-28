module Data where

import Text.Parsec (Parsec, letter, char, (<|>), many1, (<?>), satisfy, alphaNum, many, between, digit)
import qualified Text.Parsec
import Text.Parsec.Pos (SourcePos)

data Val
  = Num Int
  | Str String

instance Show Val where
  show (Num x) = show x
  show (Str x) = x

data Expr
  = Val SourcePos Val
  | Obj SourcePos [(String, Expr)]
  deriving Show

number :: Parsec String () Int
number
  = (do sign <- (do char '-'
                    return "-")
                <|> return ""
        x <- many1 $ digit
        return ((read (sign ++ x)) :: Int)) <?> "number"

symbol :: Parsec String () String
symbol
  = (do x <- (letter <|> char '_')
        ys <- many (alphaNum <|> char '_')
        return (x:ys)) <?> "symbol"

string :: Parsec String () String
string
  = (between (char '"') (char '"')
     $ many ((do char '\\'
                 char '"'
                   <|> char '\\'
                   <|> (do char 'n'
                           return '\n'))
              <|> satisfy ((/=) '"'))) <?> "String"

bom :: Parsec String () ()
bom
  = do Text.Parsec.string "\xef\xbb\xbf" <|> return "" -- UTF8 BOM, ew
       return ()

lookupi :: Expr -> Int -> Maybe Expr
lookupi (Obj _ ((k, v):xs)) x | k == show x = Just v
lookupi (Obj inf (_:xs)) x = lookupi (Obj inf xs) x
lookupi _ _ = Nothing

lookups :: Expr -> String -> Maybe Expr
lookups (Obj _   ((k, v):xs)) x | x == k = Just v
lookups (Obj inf (_:xs)) x = lookups (Obj inf xs) x
lookups _ _ = Nothing

asnum :: Val -> Int
asnum (Num x) = x

asstr :: Val -> String
asstr (Str s) = s

asval :: Expr -> Val
asval (Val _ x) = x

(!) a b = case lookups a b of
            Just x -> x

toList (Obj _ x) = x
keys (Obj _ x) = map fst x
elems (Obj _ x) = map snd x

ashow :: Expr
      -> String
ashow (Val _ x) = show x
