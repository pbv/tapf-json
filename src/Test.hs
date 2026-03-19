
module Test where

import Types
import Parser
import Pretty

import Text.Parsec
import Test.QuickCheck

-- Round-trip property:
-- parsing the result of pretty-printing should give back
-- the original JSON value
prop_roundtrip :: JValue -> Property
prop_roundtrip v 
  = parse jsonVal "tests" (prettyJson v) === Right v

instance Arbitrary JValue where
  arbitrary = sized genJson
  shrink = shrinkJson

-- generator for JSON values
genJson :: Int -> Gen JValue
genJson size
  | size > 0 = oneof [ genArray size
                     , genObject size
                     , genBasic
                     ]
  | otherwise = genBasic


genArray :: Int -> Gen JValue
genArray size = do
  n <- choose (0, size)    -- length of the list
  let size' = size `div` n -- size of sub values
  JArray <$> vectorOf n (genJson size')

genObject :: Int -> Gen JValue
genObject size = do
  n <- choose (0, size)
  let size' = size `div` n
  vs <- vectorOf n (genJson size')
  ks <- vectorOf n genNoEscape
  return (JObject $ zip ks vs)


genBasic :: Gen JValue
genBasic = oneof [ JNumber <$> arbitrary
                  , JBool <$> arbitrary
                  , JString <$> genNoEscape
                  , return JNull
                  ]

-- generator for strings of haracters w/o escapes
genNoEscape :: Gen String
genNoEscape = listOf (choose (' ', '}'))

-- shrinker for JSON values
shrinkJson :: JValue -> [JValue]
shrinkJson (JArray vs)
  = [v | v<-vs] ++  [JArray vs' | vs'<-shrink vs]
shrinkJson (JObject kvs)
  = [v | (_,v)<-kvs] ++ [JObject kvs' | kvs'<-shrink kvs]
shrinkJson (JNumber n)
  = [JNumber n' | n'<-shrink n]
shrinkJson (JString s)
  = [JString s' | s'<-shrink s]
shrinkJson _
  = []
