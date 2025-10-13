{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Web.Scotty
-- import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Data.Time (UTCTime, Day, getCurrentTime, addDays, utctDay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Exception
import Data.String (fromString)

data TaskStatus = Pending | InProgress | Completed
    deriving (Generic, Show, Eq, Read)

data TaskPriority = Low | Medium | High
    deriving (Generic, Show, Eq, Read)

derivePersistField "TaskStatus"
derivePersistField "TaskPriority"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    username String
    role String
    createdAt UTCTime
    deriving Show

Task sql=tasks
    Id sql=task_id
    name String sql=task_name
    userId Int
    createdAt UTCTime
    status TaskStatus default='Pending'
    dueDate Day
    priority TaskPriority default='Medium'
    deriving Show
|]

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=postgres password=12345 dbname=db"

-- data Task = Task
--     { task_id :: DBKey
--     , task_name :: String
--     , user_id :: Int
--     , created_at :: UTCTime
--     , status :: TaskStatus
--     , due_date :: Day
--     , priority :: TaskPriority
--     } deriving (Generic, Show)
--



main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool connStr 3 $ \pool  -> do
    liftIO $ runSqlPersistMPool (do
        runMigration migrateAll

        now <- liftIO getCurrentTime
        let in5Days = addDays 5 (utctDay now)

        taskId <- insert $ Task
            { taskName = "Learn Haskell"
            , taskUserId = 1
            , taskCreatedAt = now
            , taskDueDate = in5Days
            , taskPriority = High
            }
        --
        -- liftIO $ putStrLn $ "Created task with ID: " ++ show taskId
        -- userId <- insert $ User
        --     { userUsername = "ishto"
            -- , userRole = "cutie"
            -- , userCreatedAt = now
            -- }

        -- liftIO $ putStrLn $ "Created user with ID: " ++ show userId

        allTasks <- selectList [] [] :: SqlPersistM [Entity Task]
        mapM_ (liftIO . print . entityVal) allTasks
        ) pool

    -- scotty 3000 $ do
        -- get "/" $ do
        --     text "Server works!"
        -- get "/user/:id" $ do
        --     userId <- param "id"
        --     text $ "User: " <> userId
