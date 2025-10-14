{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Time (UTCTime, Day)
import Database.Persist.TH

data TaskStatus = Pending | InProgress | Completed
  deriving (Show, Read, Eq, Generic)
derivePersistField "TaskStatus"

instance ToJSON TaskStatus
instance FromJSON TaskStatus

data TaskPriority = Low | Medium | High
  deriving (Show, Read, Eq, Generic)
derivePersistField "TaskPriority"

instance ToJSON TaskPriority
instance FromJSON TaskPriority

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    username String
    role String
    createdAt UTCTime
    deriving Show Generic ToJSON FromJSON

Task sql=tasks
    name String sql=task_name
    userId Int
    createdAt UTCTime
    status TaskStatus default='Pending'
    dueDate Day
    priority TaskPriority default='Medium'
    deriving Show Generic FromJSON ToJSON
|]

