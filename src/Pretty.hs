--
-- Basic pretty-printing for JSON values
--
module Pretty(prettyJson) where

import Types
import Data.List (intersperse)

prettyJson :: JValue -> String
prettyJson (JString s) = show s
prettyJson (JNumber n) = show n
prettyJson (JBool b)
  = if b then "true" else "false"
prettyJson JNull = "null"
prettyJson (JArray vs)
  = "[" ++ concat (intersperse ", " $ map prettyJson vs) ++ "]"
prettyJson (JObject kvs)
  = "{" ++ concat (intersperse ", " $ map showPair kvs) ++ "}"
  where
    showPair (k,v) = show k ++ ": " ++ prettyJson  v

