module Main where
  import Data.Char
  import Control.Applicative
  import System.Environment

  -- NOTE: No support for floats
  -- NOTE: Iterating an JsonObject is slow. Implement Map
  data JsonValue
    = JsonNull
    | JsonBool Bool
    | JsonNumber Int
    | JsonString String
    | JsonArray [JsonValue]
    | JsonObject [(String, JsonValue)]
    deriving (Show, Eq)

  -- NOTE: No proper error reporting
  newtype Parser a =
    Parser { runParser :: String -> Maybe (String, a) }
  
  -- TODO: See if 'return' works implicitly
  instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

  instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) = 
      Parser $ \input -> do
        (input', f) <- p1 input
        (input'', a) <- p2 input'
        Just (input'', f a)

  instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    (Parser p1) <|> (Parser p2) =
      Parser $ \input -> p1 input <|> p2 input

  charP :: Char -> Parser Char
  charP x = Parser f
    where
      f (y:ys)
        | y == x = Just (ys, x)
        | otherwise = Nothing
      f [] = Nothing

  stringP :: String -> Parser String
  stringP input = sequenceA $ map charP input

  spanP :: (Char -> Bool) -> Parser String
  spanP f =
    Parser $ \input ->
      let (token, rest) = span f input
        in Just (rest, token)

  notNull :: Parser [a] -> Parser [a]
  notNull (Parser p) =
    Parser $ \input -> do
      (input', xs) <- p input
      if null xs
        then Nothing
        else Just (input', xs)

  -- NOTE: No escape support
  stringLiteral :: Parser String
  stringLiteral = charP '"' *> (spanP (/= '"')) <* charP '"'

  ws :: Parser String
  ws = spanP isSpace

  sepBy :: Parser a -> Parser b -> Parser [b]
  sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
      
  jsonNull :: Parser JsonValue
  jsonNull = (\_ -> JsonNull) <$> stringP "null"

  jsonBool :: Parser JsonValue
  jsonBool = f <$> (stringP "true" <|> stringP "false")
    where
      f "true" = JsonBool True
      f "false" = JsonBool False
      f _ = undefined -- NOTE: This shouldn't happen

  jsonNumber :: Parser JsonValue
  jsonNumber = f <$> notNull (spanP isDigit)
    where f ds = JsonNumber $ read ds

  jsonString :: Parser JsonValue
  jsonString = JsonString <$> stringLiteral

  jsonArray :: Parser JsonValue
  jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']')
    where
      elements = sepBy (ws *> charP ',' <* ws) jsonValue

  jsonObject :: Parser JsonValue
  jsonObject = JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}')
    where
      pair = 
        (\key _ val -> (key, val)) <$> stringLiteral <*>
        (ws *> charP ':' <* ws) <*>
        jsonValue

  jsonValue :: Parser JsonValue
  jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

  parseFile :: FilePath -> Parser a -> IO (Maybe a)
  parseFile fileName parser = do
    input <- readFile fileName
    return (snd <$> runParser parser input)

  main = do  
    args <- getArgs
    case args of
      [] -> putStrLn "No Args"
      (x:_) -> do
        out <- parseFile x jsonValue
        case out of
          Just y -> putStrLn $ show y
          _ -> putStrLn $ show out