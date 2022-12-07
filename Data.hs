module Data where

import Text.Parsec (Parsec, letter, char, (<|>), many1, (<?>), satisfy, alphaNum, many, between, digit)
import qualified Text.Parsec
import Text.Parsec.Pos (SourcePos)

data Val
  = Num Int
  | Flo Float
  | Str String

instance Show Val where
  show (Num x) = show x
  show (Str x) = x
  show (Flo x) = show x

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
                   <|> (do char 't'
                           return '\t')
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
asnum x = error $ "Expected number, got " ++ show x

asfloat :: Expr -> Float
asfloat (Val _ (Flo x)) = x
asfloat (Val _ (Num x)) = fromIntegral x
asfloat (Obj info x) = error $ "Expected float or number in {" ++ show info ++ "} but got {" ++ show x ++ "}"
asfloat (Val info x) = error $ "Expected float or number in {" ++ show info ++ "} but got {" ++ show x ++ "}"

asstr :: Val -> String
asstr (Str s) = s
asstr (Num s) = show s
asstr (Flo s) = show s

asval :: Expr -> Val
asval (Val _ x) = x
asval (Obj info x) = error $ "Expected value in {" ++ show info ++ "} but got {" ++ show x ++ "}"

(!) a b = case lookups a b of
            Just x -> x
            _ -> error $ "Table: " ++ show a ++ "\nKey: " ++ show b ++ "\nNot found"

toList (Obj _ x) = x
toList (Val info x) = error $ "Expected array/object in {" ++ show info ++ "} but got {" ++ show x ++ "}"

keys (Obj _ x) = map fst x
keys (Val info x) = error $ "Expected array/object in {" ++ show info ++ "} but got {" ++ show x ++ "}"

elems :: Expr -> [Expr]
elems (Obj _ x) = map snd x
elems (Val info x) = error $ "Expected array/object in {" ++ show info ++ "} but got {" ++ show x ++ "}"

ashow :: Expr
      -> String
ashow (Val _ x) = show x
ashow (Obj info x) = error $ "Expected value in {" ++ show info ++ "} but got {" ++ show x ++ "}"

byindex (Obj inf a) b = case length a > b of
                          True -> snd $ a !! b
                          False -> error $ "OOB: " ++ show b ++ " for " ++ show a ++ " in " ++ show inf
byindex x _ = error $ show x
