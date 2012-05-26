{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Model.Util where

import Control.Applicative
import Control.Monad

import Data.List
import Data.Maybe
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time

import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH

import Model.Definition

type SqliteIO a = PersistConfigBackend SqliteConf IO a

runDB :: ConnectionPool -> SqliteIO a -> IO a
runDB pool f = runPool dbConf f pool

dbConf :: SqliteConf
dbConf = SqliteConf "references.sqlite" 1

authorUserName :: Author -> SqliteIO Text
authorUserName author =
  case authorUser author of
     Just userKey -> userName <$> getJust userKey
     Nothing -> return $ fromMaybe (error "authorName: should not be Nothing") 
                                   (authorName author)

authorsFor :: Pub -> SqliteIO Text
authorsFor pub = do
  authors :: [Author] <- mapM getJust (pubAuthors pub)
  Text.intercalate "; " <$> mapM authorUserName authors

reviewsFor :: PubKey -> SqliteIO [Entity Review]
reviewsFor key = selectList [ReviewRefer ==. key] []

yearToUTC year = UTCTime (fromGregorian (fromIntegral year) 0 0) 0

