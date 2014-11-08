{-# LANGUAGE FlexibleContexts #-}
module Parser where

import LC

import Text.Parsec
import Text.Parsec.Text
import Data.Text as T
import Control.Applicative hiding ((<|>), many)

type TermT = Term Text


parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = between (char '(') (char ')')

expr :: Parser TermT
expr = var <|> lam <|> app

ident :: Parser Text
ident = T.pack <$> ((:) <$> letter <*> many alphaNum)

var :: Parser TermT 
var = Var <$> ident

lam :: Parser TermT
lam = parens $ do
  _ <- char '\\'
  v <- spaces *> (ident <* spaces)
  _ <- char '.'
  exp' <- spaces *> expr
  return (Lam v exp')

app :: Parser TermT
app = do
  e1 <- expr
  spaces
  e2 <- expr
  return (App e1 e2)
