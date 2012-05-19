{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
module DBModel where -- (BibEntry, bibTable, year, title) where

import Database.Persist
import Database.Persist.Store
import Database.Persist.Sqlite
import Database.Persist.TH

data PublicationType = 
  Journal | Conference | Book 
  | Misc | Article | TechReport | PhdThesis deriving (Show, Read, Eq)

-- data BibEntry = 
--   BibEntry { bibTitle :: String
--            , bibYear :: Int
--            , bibAuthors :: [String]
--            , bibPubType :: PublicationType
--            }

derivePersistField "PublicationType"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
BibEntry
   title String
   year Int
   authors [String]
   pubType PublicationType
   
ReferenceComment
  refer BibEntryKey
  title String
  text String
|]

type BibEntryKey = Key SqlPersist BibEntry


runDB :: PersistConfigBackend SqliteConf IO a -> IO a
runDB f = createPoolConfig dbConf >>= runPool dbConf f

dbConf :: SqliteConf
dbConf = SqliteConf "references.sqlite" 1


-- import Database.HaskellDB.Query
-- import Database.HaskellDB.PrimQuery
-- import Database.HaskellDB.FieldType
-- import Database.HaskellDB.Database
-- import Database.HaskellDB.DBLayout
-- import Database.HaskellDB.CodeGen

-- instance ExprType PublicationType where
--   fromHaskellType _ = (IntT, False)
-- instance ShowConstant PublicationType where
--   showConstant = IntegerLit . fromIntegral . fromEnum
-- instance GetValue PublicationType where
--   getValue insts s field = toEnum `fmap` getValue insts s field

-- tbl = mkDBDirectTable "BibTable" 
--   [ ("Title", [t|String|])
--   , ("Year", [t|Int|])
--   , ("PubType", [t|PublicationType|])
--   , ("Authors", [t|String|])
--   ]

-- mkDBDirectTable "BibTable" 
--   [ ("Title", [t|String|])
--   , ("Year", [t|Int|])
--   , ("PubType", [t|PublicationType|])
--   , ("Authors", [t|String|])
--   ]


-- mkDBDirectField "title"   [t|String|]
-- mkDBDirectField "year"    [t|Int|]
-- mkDBDirectField "pubtype" [t|PublicationType|]
-- mkDBDirectField "authors" [t|String|]

-- type a :=> b = RecCons a b
-- type a :< b = a :=> Expr b
-- type a :+ b = a b

-- infixr 2 :<
-- infixr 2 :=>
-- infixr 1 :+

-- type End = RecNil

-- type BibEntry 
--      =  Title :< String
--      :+ Year :< Int
--      :+ Pubtype :< PublicationType
--      :+ Authors :< String
--      :+ End

-- bibTable :: Table BibEntry
-- bibTable = baseTable bibTableName 
--            ( hdbMakeEntry Title 
--              # hdbMakeEntry Year 
--              # hdbMakeEntry Pubtype
--              # hdbMakeEntry Authors)
--   where bibTableName = "bibliography"