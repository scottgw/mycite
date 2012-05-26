module CommentEdit where

import Graphics.UI.Gtk

import Database.Persist
import Database.Persist.Sqlite

import DBModel

commentsFor :: ConnectionPool -> BibEntryKey -> IO [Entity ReferenceComment]
commentsFor pool key = 
  runDB pool $ selectList [ReferenceCommentRefer ==. key] []

