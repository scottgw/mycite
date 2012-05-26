{-# LANGUAGE OverloadedStrings #-}

module Bibtex where

import Control.Applicative
import Control.Monad ((<=<))

import Data.Char
import qualified Data.Text as Text
import Data.Text (Text)


import qualified Text.BibTeX.Entry as Bibtex
import qualified Text.BibTeX.Parse as Bibtex
import Text.Parsec (many1)
import Text.Parsec.String (parseFromFile)

import Database.Persist.Sqlite
import Database.Persist.Store as DB

import Model.Definition
import Model.Util
  
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

toPub :: Bibtex.T -> [AuthorKey] -> Pub
toPub t authorKeys = 
  let pubType = 
        case map toLower (Bibtex.entryType t) of
          "inproceedings" -> Conference
          "incollection" -> Journal
          "misc" -> Misc
          "book" -> Book
          "article" -> Article
          "techreport" -> TechReport
          "phdthesis" -> PhdThesis
          e -> error $ "toPub: " ++ e
      fields = Bibtex.fields t
      
      title = case lookup "title" fields <|> lookup "author" fields of
        Just t -> Text.pack t
        Nothing -> error $ show t ++ " has no title"
      
      year :: Int
      Just year = read <$> lookup "year" fields
      clean = Text.unwords . Text.words . Text.filter (not . (`elem` "\r\n"))
  in Pub (clean title) Nothing (yearToUTC year) authorKeys pubType

authors t = 
  let Just authors = lookup "author" (Bibtex.fields t)
  in authorsToList authors

uniqueBibEntries pool = do 
  es <- parseEntries "bibfile.bib"
  let mkEntryAndAuthors e = do
        let authorNames = map Text.pack $ authors e
        authorKeys <- runDB pool $ 
          mapM (\name -> either entityKey id <$> 
                         DB.insertBy (Author (Just name) Nothing)) authorNames
        return (toPub e authorKeys)
  mapM mkEntryAndAuthors es

populateDB pool = do
  es <- uniqueBibEntries pool
  runDB pool $ mapM_ DB.insert es
