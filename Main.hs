{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings #-}
{-# LANGUAGE GADTs, FlexibleContexts, EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.List

import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.Store

import Text.Regex

import CommentEdit
import DBModel
import Bibtex
import BibEntry

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  
  pool <- createPoolConfig dbConf

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
  
  entries <- fetchEntries pool
  
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
          
          regex = mkRegexWithOpts (escape searchText) False False
          searchResult authors = matchRegex regex (title ++ authors)
        
        authors <- authorsFor pool x
        
        case searchResult authors of
          Just _ -> return True
          Nothing -> return False
  treeModelFilterSetVisibleFunc searchStore searchFunc
  
  
  treeView <- bibEntryTreeView pool searchStore listStore entries 
  containerAdd container treeView
  
  widgetShowAll w

  w `onDestroy` (saveStore pool listStore >> mainQuit)

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

saveStore pool listStore = do
  entities <- listStoreToList listStore
  runDB pool $ do
    let saveEntity (Entity key val) = replace key val
    mapM_ saveEntity entities

refsContainer win = do
  scrollWin <- scrolledWindowNew Nothing Nothing
  scrolledWindowSetPolicy scrollWin PolicyAutomatic PolicyAutomatic

  container <- vBoxNew False 2
  scrolledWindowAddWithViewport scrollWin container
    
  containerAdd win scrollWin
  return container

fetchEntries :: ConnectionPool -> IO [Entity BibEntry]
fetchEntries pool = do
  runDB pool (runMigration migrateAll)
  runDB pool (selectList [] [])
