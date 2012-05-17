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

import DBModel
import Bibtex

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
        
        let title = bibEntryTitle (entityVal x)
            author = bibEntryAuthors (entityVal x)
            regex = mkRegexWithOpts searchText False False
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


bibEntryTreeView filterStore listStore entries = do  
  treeView <- treeViewNewWithModel filterStore
  
  let 
    addColumn :: CellRendererTextClass renderer => 
                 String -> Bool -> (BibEntry -> String) -> renderer -> IO ()
    addColumn title resize extractor renderer = do
      column <- treeViewColumnNew
      
      treeViewColumnPackStart column renderer False
      set column [ treeViewColumnTitle := title
                 , treeViewColumnResizable := resize
                 , treeViewColumnMaxWidth := 250
                 ]
      treeViewAppendColumn treeView column
      cellLayoutSetAttributes column renderer listStore
        (\row -> [cellText := extractor (entityVal row)])
      set renderer [cellTextEditable := False]
        
    textColumn title extractor = 
      cellRendererTextNew >>= addColumn title True extractor

    spinColumn title extractor = do
      renderer <- cellRendererSpinNew
      adjustment <- adjustmentNew 2000 0 3000 1 1 0
      set renderer [ cellRendererSpinDigits := 0
                   , cellRendererSpinAdjustment := adjustment
                   ]
      addColumn title False extractor renderer
  
    reactIter path = do
      Just iter <- treeModelGetIter listStore path
      let i = listStoreIterToIndex iter

      win <- windowNew
      widget <- refWidget listStore i
      containerAdd win widget
      widgetShowAll win
  
  textColumn "Title" bibEntryTitle
  textColumn "Authors" (intercalate "; " . bibEntryAuthors)
  spinColumn "Year" (show . bibEntryYear)
  
  on treeView rowActivated (\ path _col -> reactIter path)

  return treeView

listIterToItem :: TreeIter -> ListStore a -> IO a
listIterToItem iter store =
  let i = listStoreIterToIndex iter
  in listStoreGetValue store i
   
pack container w = boxPackStart container w PackNatural 2

data BibEntryWidget = 
  BibEntryWidget 
  { bibWidgetIdent :: BibEntryKey
  , bibWidgetTitle :: Entry
  , bibWidgetYear :: SpinButton
  }


data GlobalState = GlobalState { dirty :: Bool }

leftAlignNew = alignmentNew 0 0 0 0.5

bibEntryWidgetNew listStore i = do
  bibEntity <- listStoreGetValue listStore i
  let bibEntry@(BibEntry title year authors pub) = entityVal bibEntity
      key = entityKey bibEntity

  entry <- entryNew
  entrySetText entry title
  on entry editableChanged 
    (do 
        title' <- entryGetText entry
        let bibEntry' = bibEntry {bibEntryTitle = title'}
        listStoreSetValue listStore i (bibEntity {entityVal = bibEntry'})
    )

  adjustment <- adjustmentNew 0 0 2020 1 5 0
  spin <- spinButtonNew adjustment 1.0 0
  spinButtonSetValue spin (fromIntegral year)
  on spin editableChanged 
    (do 
        year' <- spinButtonGetValueAsInt spin
        let bibEntry' = bibEntry {bibEntryYear = year'}
        listStoreSetValue listStore i (bibEntity {entityVal = bibEntry'})
    )
  
  return $ BibEntryWidget key entry spin

saveBibEntryWidget (BibEntryWidget key title year) = runDB $ do
  title' <- liftIO $ entryGetText title
  year' <- liftIO $ spinButtonGetValue year
  
  update key
    [ BibEntryTitle =. title'
    , BibEntryYear =. round year'
    ]

refWidget listStore i = do
  container <- tableNew 2 2 False
  BibEntryWidget _ titleWidget yearWidget <- 
    bibEntryWidgetNew listStore i
  
  let attach :: WidgetClass w => Int -> Int -> w -> IO ()
      attach x y w = do 
        align <- leftAlignNew
        containerAdd align w
        tableAttach container align x (x + 1) y (y + 1)
                        [Fill] [Shrink] 5 0
      labelNew' = labelNew . Just
  
  labels <- mapM labelNew' ["Title", "Year"]
  
  let widgets = [toWidget titleWidget, toWidget yearWidget]
  
  zipWithM_ (attach 0) [0..1] labels 
  zipWithM_ (attach 1) [0..1] widgets

  return container

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
