{-
  Types for representing JSON values
-}
module Types where

-- | simplified JSON values
-- only integers,
-- no floating point numbers
data JValue
  = JString String
  | JNumber Integer
  | JBool Bool
  | JArray [JValue]
  | JObject [(String, JValue)]
  | JNull
  deriving (Eq, Show)

