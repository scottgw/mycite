{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module Model.Definition where

import Data.Text
import Data.Time

import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH

data PubType = 
  Journal 
  | Conference 
  | Book 
  | Misc 
  | Article 
  | TechReport 
  | PhdThesis 
  deriving (Show, Read, Eq)

data Rating =
  Rating 
  { ratingWriting :: Int
  , ratingContent :: Int
  } 
  deriving (Show, Read, Eq)

derivePersistField "PubType"
derivePersistField "Rating"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Author
  name Text Maybe
  user UserKey Maybe

User
  name Text
  Unique name

Pub
   title Text
   abstract Text Maybe
   date UTCTime
   authors [AuthorKey]
   pubType PubType

Review
  refer PubKey
  rating Rating
  text Text
|]

type SqlKey a = Key SqlPersist a
type AuthorKey = SqlKey Author
type PubKey = SqlKey Pub
type UserKey = SqlKey User
