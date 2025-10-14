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
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH

import Web.Scotty (scotty, get, post, queryParam, pathParam, param, text, rescue)
import qualified Web.Scotty as S

import qualified Data.Text.Lazy as T

-- import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Data.Time (UTCTime, Day, getCurrentTime, addDays, utctDay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Exception
import Data.String (fromString)
import Data.Text.Lazy (Text, pack)
import Data.Time

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

-- crud
-- endpoints:
-- GET /api/tasks?user_id=2137 -> [Task]
-- POST /api/tasks -> Task
-- PUT /api/tasks/<task_id> -> Task
-- PATCH /api/tasks/<task_id>/complete -> Nothing
-- DELETE /api/tasks/<task_id> -> Nothing

-- main :: IO ()
-- main = runStdoutLoggingT $ withPostgresqlPool connStr 3 $ \pool  -> do
--     liftIO $ runSqlPersistMPool (do
--         runMigration migrateAll
--
--         now <- liftIO getCurrentTime
--         let in5Days = addDays 5 (utctDay now)
--
--         -- taskId <- insert $ Task
--         --     { taskName = "Learn Haskell"
--         --     , taskUserId = 1
--         --     , taskCreatedAt = now
--         --     , taskDueDate = in5Days
--         --     , taskPriority = High
--         --     }
--         --
--         -- liftIO $ putStrLn $ "Created task with ID: " ++ show taskId
--         -- userId <- insert $ User
--         --     { userUsername = "ishto"
--             -- , userRole = "cutie"
--             -- , userCreatedAt = now
--             -- }
--
--         -- liftIO $ putStrLn $ "Created user with ID: " ++ show userId
--
--         allTasks <- selectList [] [] :: SqlPersistM [Entity Task]
--         mapM_ (liftIO . print . entityVal) allTasks
--         ) pool
--
--     S.scotty 3000 $ do
--         S.get "/tasks" $ do
--             uid <- S.queryParam "user_id"
--             S.text $ "GET /api/tasks" <> uid
--         S.post "/tasks/:task_id" $ do
--             taskId <- S.pathParam "task_id"
--             S.text "POST /api/tasks/" <> taskId

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool connStr 3 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ runMigration migrateAll

    scotty 3000 $ do

        S.get "/api/tasks" $ do
            uid <- queryParam "user_id" :: S.ActionM Int
            tasks <- liftIO $ flip runSqlPersistMPool pool $ selectList [TaskUserId ==. read (show uid)] []
            S.text $ "Found tasks: " <> pack (show tasks)

        S.post "/api/tasks/:task_id" $ do
            taskId <- pathParam "task_id" :: S.ActionM Int
            text $ T.pack ("POST /api/tasks/" ++ show taskId)



