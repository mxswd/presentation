{-# LANGUAGE FlexibleContexts #-}

module Slide (Slide(..), Attr(..), readSlides) where

import Text.Parsec hiding ((<|>))
import Control.Applicative
import System.Directory
import Data.Maybe
import Data.List

data Slide = Slide {
    title :: String
  , slideTitle :: String
  , slideContent :: [(String, Attr)]
  , notes :: String
  } deriving Show

data Attr = NoneAttr | KeyAttr | ItaAttr | RedAttr
  deriving Show

readSlides :: String -> IO [Slide]
readSlides path = do
  ss <- readSlideDir path
  return $ mkSlides ss

readSlideDir :: String -> IO [(Int, String, String)]
readSlideDir path = do
  fs <- getDirectoryContents path
  -- drop 2 because of "." and ".."
  flip mapM (filter ((/=) '.' . head) fs) $ \n -> do
    f <- readFile (path ++ "/" ++ n)
    let num  = read $ takeWhile (/= '_') n
    let name = takeWhile (/= '.') . drop 1 . dropWhile (/= '_') $ n
    return (num, name, f)

mkSlides :: [(Int, String, String)] -> [Slide]
mkSlides n' = exprs (sortBy (compare) n')
  where
    exprs n = flip map (map extData n) $ \(fiu, fn, fc, fs) ->
              Slide fiu fn (mkdAt fiu fs) fc
    extData :: (Int, String, String) -> (String, String, String, String)
    extData (num, fileName, content) = (fileName, ts, sl, nl)
      where
        splitAt' c w x = (take n x, drop (n + w) x)
          where n = fromJust $ findIndex (== c) x
        (ts, rest)  = splitAt' '\n' 1 content
        (sl, rest') = splitAt' '-' 4 rest
        nl = rest'

mkdAt :: String -> String -> [(String, Attr)]
mkdAt fnam s = v (parse mkParse fnam s)
  where
    v (Right v') = v'
    v (Left e) = error $ show e

highlight ke at = try $ do
  v <- between (char ke) (char ke) (many1 (letter <|> space))
  return (v, at)

bold = highlight '*' KeyAttr
redu = highlight '+' RedAttr
ital = highlight '&' ItaAttr
-- cros = highlight '-' itaAttr

word = try $ do
  v <- many1 (alphaNum <|> char '-' <|> char '?' <|> char '('
              <|> char ')' <|> char '.' <|> char ':' <|> char '/'
              <|> char '~' <|> char '=' <|> char ',' <|> char ';'
              <|> char '<' <|> char '>' <|> char '\'' <|> char '['
              <|> char ']' <|> char '"' <|> char '|' <|> char '\\'
              <|> char '@' <|> char '_' <|> char '%' <|> char '^'
              <|> char '∧' <|> char '∨' <|> char '!' <|> char '{'
              <|> char '}' <|> char '$')
  return (v, NoneAttr)
newl = do
  newline
  return ("\n", NoneAttr)
spacex = do
  space
  return (" ", NoneAttr)

mkParse = manyTill (bold <|> ital <|> redu <|> newl <|> spacex <|> word) eof
