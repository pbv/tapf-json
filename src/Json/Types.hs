{-
  Types for representing JSON value
-}
module Json.Types where

import Data.List (intersperse)

-- | a simplified JSON values:
-- no floating point numbers
data JValue
  = JString String
  | JNumber Integer
  | JBool Bool
  | JArray [JValue]
  | JObject [(String, JValue)]
  | JNull
  deriving Eq

instance Show JValue where
   show = showJson

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
