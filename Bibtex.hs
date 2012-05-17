{-# LANGUAGE OverloadedStrings #-}

module Bibtex where

import Control.Applicative

import Data.Char

import qualified Text.BibTeX.Entry as Bibtex
import qualified Text.BibTeX.Parse as Bibtex
import Text.Parsec (many1)
import Text.Parsec.String (parseFromFile)

import Database.Persist.Sqlite
import Database.Persist.Store as DB

import DBModel

runDB :: PersistConfigBackend SqliteConf IO a -> IO a
runDB f = createPoolConfig dbConf >>= runPool dbConf f

dbConf :: SqliteConf
dbConf = SqliteConf "references.sqlite" 1

  
authorsToList :: String -> [String]
authorsToList = reverse . go [] . words
  where
    go acc [] = acc
    go acc ws = 
      let (author, rest) = break (== "and") ws
          acc' = unwords author : acc
      in case rest of
        [] -> acc'
        _:rest' -> go acc' rest'

parseEntries :: FilePath -> IO [Bibtex.T]
parseEntries file = do
  entriesEi <- parseFromFile (many1 Bibtex.entry) file 
  return $ case entriesEi of
    Left e -> error $ show e
    Right es -> es

toBibEntry :: Bibtex.T -> BibEntry
toBibEntry t = 
  let pubType = 
        case map toLower (Bibtex.entryType t) of
          "inproceedings" -> Conference
          "incollection" -> Journal
          "misc" -> Misc
          "book" -> Book
          "article" -> Article
          "techreport" -> TechReport
          "phdthesis" -> PhdThesis
          e -> error $ "toBibEntry: " ++ e
      fields = Bibtex.fields t
      Just authors = lookup "author" fields
      title = case lookup "title" fields <|> lookup "author" fields of
        Just t -> t
        Nothing -> error $ show t ++ " has no title"
      Just year = lookup "year" fields
      clean = unwords . words . filter (/= '\n') . filter (/= '\r')
  in BibEntry (clean title) (read year) (authorsToList authors) pubType

uniqueBibEntries = do 
  es <- parseEntries "bibfile.bib"
  return (map toBibEntry es)

populateDB = do
  es <- uniqueBibEntries
  runDB $ mapM_ DB.insert es
