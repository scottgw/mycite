{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyCite.Model.Util where

import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time

import Database.Persist
import Database.Persist.GenericSql
import Database.Persist.Store
import Database.Persist.TH

import MyCite.Model.Definition

type Persist a = SqlPersist IO a

runDB :: ConnectionPool -> Persist a -> IO a
runDB = flip runSqlPool

authorUserName :: Author -> Persist Text
authorUserName author =
  case authorUser author of
     Just userKey -> userName <$> getJust userKey
     Nothing -> return $ fromMaybe (error "authorName: should not be Nothing") 
                                   (authorName author)

authorsFor :: Pub -> Persist Text
authorsFor pub = do
  authors :: [Author] <- mapM getJust (pubAuthors pub)
  Text.intercalate "; " <$> mapM authorUserName authors

reviewsFor :: PubKey -> Persist [Entity Review]
reviewsFor key = selectList [ReviewRefer ==. key] []

yearToUTC year = UTCTime (fromGregorian (fromIntegral year) 0 0) 0