--
-- Basic pretty-printing for JSON values
--
module Pretty where

import Types
import Data.List (intersperse)

showJson :: JValue -> String
showJson (JString s) = show s
showJson (JNumber n) = show n
showJson (JBool b) = if b then "true" else "false"
showJson JNull = "null"
showJson (JArray vs)
  = "[" ++ concat (intersperse ", " $ map showJson vs) ++ "]"
showJson (JObject kvs)
  = "{" ++ concat (intersperse ", " $ map showPair kvs) ++ "}"
  where
    showPair (k,v) = show k ++ ": " ++ showJson v

