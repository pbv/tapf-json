
module Test where

import Types
import Parser
import Pretty

import Text.Parsec
import Test.QuickCheck

--- properties

prop_roundtrip :: JValue -> Property
prop_roundtrip v =
  parse jsonVal "test" (showJson v) === Right v


---- generators

instance Arbitrary JValue where
  arbitrary = sized genJson
  shrink = shrinkJson

shrinkJson (JNumber n)= map JNumber $ shrink n
shrinkJson (JString s) = map JString $ shrink s
shrinkJson (JArray vs)
  = map JArray $ shrinkList shrinkJson vs
shrinkJson (JObject kvs)
  = map JObject $ shrinkList (\(k,v) -> [(k, v') | v'<-shrinkJson v] ++
                                        [(k',v) | k'<-shrink k]) kvs
shrinkJson _           = []


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


