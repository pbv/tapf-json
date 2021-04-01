{-
  A simplified parser for JSON values:
  1) no floating point numbers;
  2) no handling of escape characters in string literals

  Pedro Vasconcelos, 2017
-}
module Parser where

import           Types
import           Text.Parsec
import           Data.Char (isPrint, isDigit)

-- | specialized type for string parser with no user state
type Parser = Parsec String ()

--
-- Auxiliary definitions
--
-- | accept trailing spaces, newlines etc. after some parser
lexeme :: Parser a -> Parser a
lexeme p = do v<-p; spaces; return v

-- | parser a separator or parenthesis
symbol :: String -> Parser String
symbol s = lexeme (string s)


-- | parse a boolean value
bool :: Parser Bool
bool = do symbol "true"; return True
       <|> do symbol "false"; return False

-- | parse a decimal integer with optional sign
integer :: Parser Integer
integer = lexeme (do
  s <- option 1 sign
  n <- read <$> many1 (satisfy isDigit)
  return (s * n)
  <?> "number")

sign = do char '+'; return 1
       <|>
       do char '-'; return (-1)

-- | parse a string literal
stringLit :: Parser String
stringLit = lexeme $ do
  char '\"'
  s <- many (satisfy (\x -> isPrint x && x/='\"'))
  char '\"'
  return s



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
  <|> do n <- integer; return (JNumber n)
  <|> do b <- bool; return (JBool b)
  <|> do s <- stringLit; return (JString s)
  <|> do arr <- brackets (jsonVal `sepBy` comma); return (JArray arr)
  <|> do kvs <- braces (keyVal `sepBy` comma); return (JObject kvs)


-- | key-value pair
keyVal :: Parser (String, JValue)
keyVal = do
  k <- stringLit
  symbol ":"
  v <- jsonVal
  return (k,v)
            
