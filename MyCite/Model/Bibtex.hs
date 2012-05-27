{-# LANGUAGE OverloadedStrings #-}
module MyCite.Model.Bibtex where

import Control.Applicative
import Control.Monad ((<=<))

import Data.Char
import qualified Data.Text as Text
import Data.Text (Text)

import qualified Text.BibTeX.Entry as Bibtex
import qualified Text.BibTeX.Parse as Bibtex
import Text.Parsec (many1)
import Text.Parsec.String (parseFromFile)

import Database.Persist.Store

import MyCite.Model.Definition
import MyCite.Model.Util

-- | Run the standard BibTeX parser over a file.
parseEntries :: FilePath -> IO [Bibtex.T]
parseEntries file = do
  entriesEi <- parseFromFile (many1 Bibtex.entry) file 
  return $ case entriesEi of
    Left e -> error $ show e
    Right es -> es

-- | Breaks the traditional BibTeX-style author name (a list of names
-- separated by 'and') into a list of names.
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


-- | Transforms a BibTex entry into a publication entry, given a list
-- of author keys.
toPub :: Bibtex.T -- ^ BibTeX entry
         -> [AuthorKey] -- ^ Author keys
         -> Pub
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

-- | Extract a list of 
authors :: Bibtex.T -> [Text]
authors t = 
  let Just authors = lookup "author" (Bibtex.fields t)
  in map Text.pack $ authorsToList authors

-- | Insert all entries in a list.
buildPubs :: [Bibtex.T] -> Persist [Pub]
buildPubs entries =  mapM buildPub entries

-- | Insert a given entry by first parsing all of the authors and inserting
-- them, then using those insertion keys to build the publication.
buildPub :: Bibtex.T -> Persist Pub
buildPub e =
  let insertAuthor name = 
        either entityKey id <$> insertBy (Author (Just name) Nothing)
  in toPub e <$> mapM insertAuthor (authors e)