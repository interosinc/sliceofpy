{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Data.Slice.Parser.Internal where

import Control.Applicative ( Alternative( (<|>) )
                           , empty
                           , optional
                           , some
                           )
import Control.Monad       ( void )
import Data.Char           ( isDigit )

newtype Parser a = Parser { runParser :: String -> [(String, a)] }
  deriving Functor

instance Applicative Parser where
  pure x = Parser $ \s -> [(s, x)]
  Parser f <*> Parser g = Parser $ \s -> case f s of
    [(s', f')] -> fmap f' <$> g s'
    _          -> []

instance Alternative Parser where
  empty = Parser $ const []
  Parser f <|> Parser g = Parser $ \s -> case f s of
    [x] -> [x]
    _   -> g s

instance Monad Parser where
  Parser x >>= f = Parser $ \s -> case x s of
    [(s', a)] -> let Parser pb = f a in pb s'
    _         -> []

parseSlice :: String -> Maybe (Maybe Int, Maybe Int, Maybe Int)
parseSlice s = case runParser slice s of
  [("", x)] -> Just x
  _         -> Nothing

slice :: Parser (Maybe Int, Maybe Int, Maybe Int)
slice = sliceABC <|> sliceAB <|> sliceAC <|> sliceBC
  <|> sliceA <|> sliceB <|> sliceC
  <|> sliceEmpty

sliceABC :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceABC = (,,)
  <$> (Just <$> (int <* void (char ':')))
  <*> (Just <$> (int <* void (char ':')))
  <*> (Just <$> int)

sliceAB :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceAB = (,,Nothing)
  <$> (Just <$> int)
  <*> (Just <$> (void (char ':') *> int <* optional (void (char ':'))))

sliceAC :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceAC = (,Nothing,)
  <$> (Just <$> (int <* void (string "::")))
  <*> (Just <$> int)

sliceBC :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceBC = (Nothing,,)
  <$> (Just <$> (void (char ':') *> int))
  <*> (Just <$> (void (char ':') *> int))

sliceA :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceA = (,Nothing,Nothing) . Just <$> (int <* void (char ':' >> optional (char ':')))

sliceB :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceB = (Nothing,,Nothing) . Just <$> (void (char ':') *> int <* optional (char ':'))

sliceC :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceC = (Nothing, Nothing,) . Just <$> (void (string "::") *> int)

sliceEmpty :: Parser (Maybe Int, Maybe Int, Maybe Int)
sliceEmpty = (Nothing, Nothing, Nothing) <$ void (string "::")

int :: Parser Int
int = do
  neg <- optional $ char '-'
  n   <- read <$> some digit
  case neg of
    Just _  -> pure $ negate n
    Nothing -> pure n

digit :: Parser Char
digit = satisfy isDigit

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string ""     = pure ""
string (c:cs) = (:) <$> char c <*> string cs

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f ""     = []
    f (c:cs) = [(cs, c) | p c]

