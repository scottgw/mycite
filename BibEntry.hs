module BibEntry where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.List
import Data.Maybe

import Graphics.UI.Gtk

import Database.Persist as DB
-- import Database.Persist.Sqlite

import DBModel
import CommentEdit

bibEntryTreeView pool filterStore listStore entries = do  
  treeView <- treeViewNewWithModel filterStore
  
  let 
    addColumn :: CellRendererTextClass renderer => 
                 String -> Bool -> (Entity BibEntry -> IO String) -> 
                 renderer -> IO ()
    addColumn title resize extractor renderer = do
      column <- treeViewColumnNew
      
      treeViewColumnPackStart column renderer False
      set column [ treeViewColumnTitle := title
                 , treeViewColumnResizable := resize
                 , treeViewColumnMaxWidth := 250
                 ]
      treeViewAppendColumn treeView column
      cellLayoutSetAttributes column renderer listStore
        (\row -> [cellText :=> extractor row])
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
      widget <- refWidget pool listStore i
      containerAdd win widget
      widgetShowAll win
  
  textColumn "Title" (return . bibEntryTitle . entityVal)
  textColumn "Authors" (authorsFor pool)
  textColumn "Comments" 
    (\e -> show <$> length <$> commentsFor pool (entityKey e))
  spinColumn "Year" (return . show . bibEntryYear . entityVal)
  
  on treeView rowActivated (\ path _col -> reactIter path)
  
  return treeView

-- authorsFor :: ConnectionPool -> Entity BibEntry -> IO String
authorsFor pool e = runDB pool $ do
  authorMbs <- mapM DB.get (bibEntryAuthors (entityVal e))
  -- log authors that weren't found in DB
  let authorNames = intercalate "; " . map authorName . catMaybes
  return (authorNames authorMbs)

listIterToItem :: TreeIter -> ListStore a -> IO a
listIterToItem iter store =
  let i = listStoreIterToIndex iter
  in listStoreGetValue store i
   
pack container w = boxPackStart container w PackNatural 2

data BibEntryWidget = 
  BibEntryWidget 
  { bibWidgetIdent :: Entity BibEntry
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
  
  return $ BibEntryWidget bibEntity entry spin

authorsWidget :: IO ()
authorsWidget = do
  return ()

saveBibEntryWidget pool (BibEntryWidget bibEntity title year) = runDB pool $ do
  title' <- liftIO $ entryGetText title
  year' <- liftIO $ spinButtonGetValue year
  
  update (entityKey bibEntity)
    [ BibEntryTitle =. title'
    , BibEntryYear =. round year'
    ]

refWidget pool listStore i = do
  table <- tableNew 2 4 False
  BibEntryWidget bibEntity titleWidget yearWidget <- 
    bibEntryWidgetNew listStore i
  
  let attach :: WidgetClass w => Int -> Int -> w -> IO ()
      attach x y w = do 
        align <- leftAlignNew
        containerAdd align w
        tableAttach table align x (x + 1) y (y + 1)
                        [Fill] [Shrink] 5 0
      labelNew' = labelNew . Just
  
      commentList = do
        comments <- commentsFor pool (entityKey bibEntity)
        commentStore <- listStoreNew comments
        
        commentView <- treeViewNewWithModel commentStore
        
        titleColumn <- treeViewColumnNew
        
        titleRender <- cellRendererTextNew
        
        treeViewColumnPackStart titleColumn titleRender False
        treeViewAppendColumn commentView titleColumn
        -- set titleColumn [treeViewColumnTitle := "Comment"]
        set commentView [treeViewHeadersVisible := False]
        cellLayoutSetAttributes titleColumn titleRender commentStore
          (\row -> [cellText := referenceCommentTitle (entityVal row)])
        
        return commentView
  
  labels <- mapM labelNew' ["Title", "Year"]
  
  let widgets = [toWidget titleWidget, toWidget yearWidget]
  
  zipWithM_ (attach 0) [0..1] labels 
  zipWithM_ (attach 1) [0..1] widgets
  
  comments <- commentList
  
  commentLabel <- labelNew (Just "Comments")
  
  tableAttach table commentLabel 0 1 2 3 [Fill] [Shrink] 5 0
  tableAttach table comments 0 2 3 4 [Fill] [Shrink] 5 0
  
  return table