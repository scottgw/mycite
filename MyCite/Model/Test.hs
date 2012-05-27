{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative

import Database.Persist
import Database.Persist.Sqlite

import MyCite.Model.Definition
import MyCite.Model.Util

main = do
  pool <- createSqlitePool "references.sqlite" 10
  titles <- runDB pool $
    map (pubTitle . entityVal) <$> selectList [] []
  print titles