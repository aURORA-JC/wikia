module Utils where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data
import Parsing


(%) a b = case lookups a b of
            Just x -> ashow x
            _ -> error $ "Table: " ++ show a ++ "\nKey:" ++ show b ++ "\nNot found"

(%%) :: Expr
     -> String
     -> H.Html
(%%) a b = H.preEscapedToHtml $ a % b
