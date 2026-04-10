
module Test where

import Types
import Parser
import Pretty

import Text.Parsec (parse)
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
  ks <- vectorOf n arbitrary 
  return (JObject $ zip ks vs)


genBasic :: Gen JValue
genBasic = oneof [ JNumber <$> arbitrary
                  , JBool <$> arbitrary
                  , JString <$> arbitrary 
                  , pure JNull
                  ]


shrinkJson :: JValue -> [JValue]
shrinkJson (JNumber n) = JNumber <$> shrink n
  -- [JNumber n' | n'<-shrink n]
  -- map JNumber (shrink n)

shrinkJson (JBool b) = JBool <$> shrink b
shrinkJson (JString s) = JString <$> shrink s
shrinkJson (JArray vs)
  = vs ++ (JArray <$> shrinkList shrinkJson vs)
shrinkJson (JObject kvs) 
  = map (JString . fst) kvs
    ++
    map snd kvs 
    ++
    (JObject <$> shrinkList shrinkPair kvs)
  where 
    shrinkPair :: (String,JValue) -> [(String,JValue)]
    shrinkPair (k,v) = 
       [(k',v) | k'<-shrink k] ++ 
       [(k,v') |v'<-shrinkJson v]
shrinkJson JNull = []