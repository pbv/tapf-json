{-
  Types for representing JSON value
-}
module Json.Types where

-- | a simplified JSON values:
-- no floating point numbers
data JValue
  = JString String
  | JNumber Integer
  | JBool Bool
  | JArray [JValue]
  | JObject [(String, JValue)]
  | JNull
  deriving Show


