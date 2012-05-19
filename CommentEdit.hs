module CommentEdit where

import Graphics.UI.Gtk

import Database.Persist
import Database.Persist.Sqlite

import DBModel

commentsFor :: BibEntryKey -> IO [Entity ReferenceComment]
commentsFor key = runDB $ selectList [ReferenceCommentRefer ==. key] []

