-- a low quality JSON parser for haskell, we use it only because aeson is bad with errors, only supports a subset of JSON

module JSON where

import Text.Parsec ((<|>), (<?>), satisfy, char, letter, alphaNum, digit, between, many, many1, eof, space, sepEndBy, sepEndBy1, Parsec)
import Text.Parsec.Prim (getPosition)

import Data

commentNext
  = do many
         $ satisfy
         $ (/=) '*'
       char '*'
       (do char '/'
           return ())
         <|> commentNext

comment
  = (do char '/'
        (do char '/'
            many
              $ satisfy
              $ (/=) '\n'
            (do char '\n'
                return ()) <|> eof)
          <|> (do char '*'
                  commentNext)) <?> "comment"

-- nom nom
eat
  = many ((do space
              return ())
          <|> comment)

sep
  = do char ','
       eat

thing :: Char
      -> Char
      -> Parsec String () (String, Expr)
      -> Parsec String () Expr
thing open close thingie
  = do pos <- getPosition
       char open
       eat
       x <- (do char ','
                eat
                thingie `sepEndBy1` sep)
            <|> thingie `sepEndBy` sep
       char close
       eat
       return
         $ Obj pos x

object :: Parsec String () Expr
object
  = (thing '{' '}'
     $ do key <- string <|> symbol
          eat
          char ':'
          eat
          val <- value
          eat
          return (key, val)) <?> "object"

array :: Parsec String () Expr
array
  = (thing '[' ']'
     $ do val <- value
          eat
          return ("", val)) <?> "array"

value :: Parsec String () Expr
value
  = (object
     <|> array
     <|> do pos <- getPosition
            x <- (string >>= return . Str)
                 <|> (do a <- number
                         (do char '.'
                             b <- number
                             return $ Flo $ read (show a ++ "." ++ show b))
                           <|> (return $ Num a))
                 <|> (symbol >>= return . Str)
            return
              $ Val pos x) <?> "expression"

json :: Parsec String () Expr
json
  = do bom
       eat
       x <- value
       eat
       eof
       return x
