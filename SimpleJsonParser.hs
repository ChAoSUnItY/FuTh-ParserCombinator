{-# LANGUAGE LambdaCase #-}

import Control.Applicative
  ( Alternative
      ( empty,
        many,
        some,
        (<|>)
      ),
  )
import Data.Char
  ( isAlpha,
    isNumber,
  )

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f p = Parser $ \input -> do
    (v, remain) <- runParser p input
    pure (f v, remain)

instance Applicative Parser where
  pure :: a -> Parser a
  pure v = Parser $ \input -> Just (v, input)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \input -> do
    (f, remain) <- runParser p1 input
    (v, remain') <- runParser p2 remain
    pure (f v, remain')

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 = Parser $ \input ->
    case runParser p1 input of
      Nothing -> runParser p2 input
      Just (v, remain) -> Just (v, remain)

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \case
  [] -> Nothing
  (x : xs) ->
    if p x
      then Just (x, xs)
      else Nothing

option :: a -> Parser a -> Parser a
option x p = p <|> pure x

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p d = (:) <$> p <*> many (d *> p) <|> pure []

ws :: Parser String
ws = many (char ' ' <|> char '\n')

-- === Actual Parsers === --
digit :: Parser Char
digit = satisfy isNumber

stringLiteral :: Parser String
stringLiteral = ws *> char '"' *> content <* char '"'
  where
    content = many $ satisfy (/= '"')

data JsonValue
  = JsonArray [JsonValue]
  | JsonBoolean Bool
  | JsonNull
  | JsonNumber Int
  | JsonObject [(String, JsonValue)]
  | JsonString String
  deriving (Show)

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (ws *> char '[' *> ws *> elements <* ws <* char ']')
  where
    elements = sepBy jsonValue (ws *> char ',' <* ws)

jsonBoolean :: Parser JsonValue
jsonBoolean = JsonBoolean . parseBool <$> (ws *> (string "true" <|> string "false"))
  where
    parseBool "true" = True
    parseBool "false" = False
    parseBool _ = undefined

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ (ws *> string "null")

jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . read <$> (ws *> ((++) <$> option "" (string "-") <*> some digit))

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (ws *> char '{' *> ws *> pairs <* ws <* char '}')
  where
    pairs = sepBy ((\key _ value -> (key, value)) <$> stringLiteral <*> (ws *> char ':' <* ws) <*> jsonValue) (ws *> char ',' <* ws)

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonValue :: Parser JsonValue
jsonValue =
  jsonArray
    <|> jsonBoolean
    <|> jsonNull
    <|> jsonNumber
    <|> jsonObject
    <|> jsonString

-- === Helper Functions === --

readJson :: String -> IO (Maybe JsonValue)
readJson filepath = do
  content <- readFile filepath
  return (fst <$> runParser jsonValue content)
