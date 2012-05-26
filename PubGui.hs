module PubGui where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.List
import Data.Maybe
import Data.Time
import qualified Data.Text as Text
import Data.Text (Text)

import Graphics.UI.Gtk

import Database.Persist as DB
-- import Database.Persist.Sqlite

import Model.Definition
import Model.Util
import CommentEdit

pubTreeView pool filterStore listStore entries = do  
  treeView <- treeViewNewWithModel filterStore
  
  let 
    addColumn :: CellRendererTextClass renderer => 
                 String -> Bool -> (Entity Pub -> IO Text) -> 
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
        (\row -> [cellText :=> Text.unpack <$> extractor row])
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
  
  textColumn "Title" (return . pubTitle . entityVal)
  textColumn "Authors" (runDB pool . authorsFor . entityVal)
  textColumn "Reviews" 
    (return . Text.pack 
            . show 
            . length 
            <=< runDB pool 
            . reviewsFor 
            . entityKey)
  spinColumn "Year" (return . Text.pack . show . pubDate . entityVal)
  
  on treeView rowActivated (\ path _col -> reactIter path)
  
  return treeView


listIterToItem :: TreeIter -> ListStore a -> IO a
listIterToItem iter store =
  let i = listStoreIterToIndex iter
  in listStoreGetValue store i
   
pack container w = boxPackStart container w PackNatural 2

data PubWidget = 
  PubWidget 
  { bibWidgetIdent :: Entity Pub
  , bibWidgetTitle :: Entry
  , bibWidgetYear :: SpinButton
  }


data GlobalState = GlobalState { dirty :: Bool }

leftAlignNew = alignmentNew 0 0 0 0.5

pubWidgetNew listStore i = do
  bibEntity <- listStoreGetValue listStore i
  let pub@(Pub title abstract date authors pubType) = entityVal bibEntity
      key = entityKey bibEntity
  
  entry <- entryNew
  entrySetText entry (Text.unpack title)
  on entry editableChanged 
    (do 
        title' <- entryGetText entry
        let pub' = pub {pubTitle = Text.pack title'}
        listStoreSetValue listStore i (bibEntity {entityVal = pub'})
    )
  
  adjustment <- adjustmentNew 0 0 2020 1 5 0
  spin <- spinButtonNew adjustment 1.0 0
  spinButtonSetValue spin ((\(a,_,_) -> fromIntegral a) $ 
                           toGregorian (utctDay date))
  on spin editableChanged 
    (do 
        year <- spinButtonGetValueAsInt spin
        let pub' = pub {pubDate = yearToUTC year}
        listStoreSetValue listStore i (bibEntity {entityVal = pub'})
    )
  
  return $ PubWidget bibEntity entry spin

authorsWidget :: IO ()
authorsWidget = do
  return ()

savePubWidget pool (PubWidget bibEntity title year) = runDB pool $ do
  title' <- liftIO $ entryGetText title
  year' <- liftIO $ spinButtonGetValue year
  
  update (entityKey bibEntity)
    [ PubTitle =. Text.pack title'
    , PubDate =. yearToUTC (round year')
    ]

refWidget pool listStore i = do
  table <- tableNew 2 4 False
  PubWidget bibEntity titleWidget yearWidget <- 
    pubWidgetNew listStore i
  
  let attach :: WidgetClass w => Int -> Int -> w -> IO ()
      attach x y w = do 
        align <- leftAlignNew
        containerAdd align w
        tableAttach table align x (x + 1) y (y + 1)
                        [Fill] [Shrink] 5 0
      labelNew' = labelNew . Just
  
      commentList = do
        comments <- runDB pool $ reviewsFor (entityKey bibEntity)
        commentStore <- listStoreNew comments
        
        commentView <- treeViewNewWithModel commentStore
        
        titleColumn <- treeViewColumnNew
        
        titleRender <- cellRendererTextNew
        
        treeViewColumnPackStart titleColumn titleRender False
        treeViewAppendColumn commentView titleColumn
        -- set titleColumn [treeViewColumnTitle := "Comment"]
        set commentView [treeViewHeadersVisible := False]
        cellLayoutSetAttributes titleColumn titleRender commentStore
          (\row -> [cellText := Text.unpack $ Text.take 25 $ reviewText (entityVal row)])
        
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