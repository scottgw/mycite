{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.List

import Database.Persist
import Database.Persist.Sqlite

import Text.Regex

import CommentEdit
import DBModel
import Bibtex
import BibEntry

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  
  w <- windowNew
  set w [ windowTitle := "MyCite"
        , windowDefaultWidth := 800
        , windowDefaultHeight := 600
        ]
  vbox <- vBoxNew False 0
  containerAdd w vbox
  
  menuBar <- menu w
  
  searchEntry <- entryNew
  
  boxPackStart vbox menuBar PackNatural 1
  boxPackStart vbox searchEntry PackNatural 1
  
  container <- refsContainer vbox
  
  entries <- fetchEntries
  
  listStore <- listStoreNew entries
  searchStore <- treeModelFilterNew listStore []
  on searchEntry editableChanged (treeModelFilterRefilter searchStore)
  
  let searchFunc iter = do
        x <- listIterToItem iter listStore
        searchText <- entryGetText searchEntry
        
        let
          -- Escaping regex special character
          escape ('\\':xs) = "\\\\" ++ escape xs
          escape (x:xs) = x : escape xs
          escape [] = []
          title = bibEntryTitle (entityVal x)
          author = bibEntryAuthors (entityVal x)
          regex = mkRegexWithOpts (escape searchText) False False
          searchResult = matchRegex regex (title ++ concat author)
        
        case searchResult of
          Just _ -> return True
          Nothing -> return False
  treeModelFilterSetVisibleFunc searchStore searchFunc
  
  
  treeView <- bibEntryTreeView searchStore listStore entries 
  containerAdd container treeView
  
  widgetShowAll w

  w `onDestroy` (saveStore listStore >> mainQuit)

  mainGUI

menu win = do
  menuBar <- menuBarNew
  fileItem <- menuItemNewWithMnemonic "_File"
  menuShellAppend menuBar fileItem
  
  fileMenu <- menuNew
  quitItem <- imageMenuItemNewFromStock stockQuit
  menuShellAppend fileMenu quitItem
  menuItemSetSubmenu fileItem fileMenu
  
  on quitItem menuItemActivate (widgetDestroy win)
  
  return menuBar

saveStore listStore = do
  entities <- listStoreToList listStore
  runDB $ do
    let saveEntity (Entity key val) = replace key val
    mapM_ saveEntity entities

refsContainer win = do
  scrollWin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyAutomatic

  container <- vBoxNew False 2
  scrolledWindowAddWithViewport scrollWin container
    
  containerAdd win scrollWin
  return container

fetchEntries :: IO [Entity BibEntry]
fetchEntries = do
  runDB (runMigration migrateAll)
  runDB (selectList [] [])
