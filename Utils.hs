module Utils where

import Data.Text (unpack)
import qualified Data.Aeson as Aeson

ashow :: Aeson.Value
      -> String
ashow (Aeson.String x) = unpack x
ashow (Aeson.Number x) = show x
ashow (Aeson.Bool x) = show x

aobj :: Aeson.Value
     -> Aeson.Object
aobj (Aeson.Object x) = x
