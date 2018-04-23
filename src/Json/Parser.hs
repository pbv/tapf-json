{-
  A simplified parser for JSON values:
  1) no floating point numbers;
  2) no handling of escape characters in string literals

  Pedro Vasconcelos, 2017
-}
module Json.Parser where

import           Json.Types
import           Text.Parsec
import qualified Text.Parsec.Token as P
import           Text.Parsec.Language (emptyDef)
import           Data.Char (isDigit)

-- | specialized type for string parser with no user state
type Parser = Parsec String ()

--
-- Auxiliary definitions
--
-- | accept trailing spaces, newlines etc. after some parser
lexeme :: Parser a -> Parser a
lexeme p = do v<-p; spaces; return v

-- | parser a separator or parenthesis
-- use try to ensure that no input is consumed if string does not match
symbol :: String -> Parser String
symbol s = lexeme (try (string s)) <?> s


-- | parse a boolean value
bool :: Parser Bool
bool = do symbol "true"; return True
       <|> do symbol "false"; return False

-- | create a token parser
lexer = P.makeTokenParser emptyDef
stringLit = P.stringLiteral lexer
integer = P.integer lexer
               

{-
-- | parse a decimal integer with optional sign
integer :: Parser Integer
integer = lexeme (do
  s <- option 1 sign
  n <- read <$> many1 (satisfy isDigit)
  return (s * n)
  <?> "number")

-- | parse a string literal
stringLit :: Parser String
stringLit = lexeme $ do
  char '\"'
  s <- many (satisfy (/='\"'))
  char '\"'
  return s
-}



-- | require brackets or braces around a parser; uses
-- between :: Parser open -> Parser close -> Parser a -> Parser a
-- 
brackets, braces :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")
braces   = between (symbol "{") (symbol "}")

-- | single comma
comma = symbol ","

-- | parser for a Json value
jsonVal :: Parser JValue
jsonVal
  = do symbol "null"; return JNull
  <|> JNumber <$> integer
  <|> JBool <$> bool
  <|> JString <$> stringLit
  <|> JArray <$> brackets (jsonVal `sepBy` comma)
  <|> JObject <$> braces (keyVal `sepBy` comma)


-- | key-value pair
keyVal :: Parser (String, JValue)
keyVal = do
  k <- stringLit
  symbol ":"
  v <- jsonVal
  return (k,v)
            
