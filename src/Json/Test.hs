
module Json.Test where

import Json.Types
import Json.Parser
import Text.Parsec
import Test.QuickCheck

--- properties

prop_roundtrip :: JValue -> Bool
prop_roundtrip v =
  parse jsonVal "test" (show v) == Right v


---- generators

instance Arbitrary JValue where
  arbitrary = sized genJson

genJson :: Int -> Gen JValue
genJson n
  | n >  0    = oneof [genArray n, genObj n]
  | otherwise = genSimple


genArray size =  do
      n <- choose (2, 10)
      let size' = size `div` n
      JArray <$> vectorOf n (genJson size')
      
genObj size =  do
      n <- choose (2, 10)
      let size' = size `div` n
      vs <- vectorOf n (genJson size')
      ks <- vectorOf n arbitrary
      return (JObject $ zip ks vs)

genSimple = oneof [ JNumber <$> arbitrary
                  , JString <$> arbitrary
                  , JBool <$> arbitrary
                  , return JNull
                  ]


