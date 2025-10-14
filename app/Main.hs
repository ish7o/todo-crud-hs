{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

{-# LANGUAGE DeriveAnyClass #-}
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

import Web.Scotty (scotty, get, post, queryParam, pathParam, param, text, json)
import qualified Web.Scotty as S

import qualified Data.Text.Lazy as T

import Network.HTTP.Types.Status (status200, status404, status204)

import Data.Aeson (ToJSON, FromJSON, object, (.=))
import GHC.Generics
import Data.Time (UTCTime, Day, getCurrentTime, addDays, utctDay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Exception
import Data.String (fromString)
import Data.Text.Lazy (Text, pack)
import Data.Time
import Data.Int (Int64)

data TaskStatus = Pending | InProgress | Completed
    deriving (Generic, Show, Eq, Read)

instance ToJSON TaskStatus
instance FromJSON TaskStatus

data TaskPriority = Low | Medium | High
    deriving (Generic, Show, Eq, Read)

instance ToJSON TaskPriority
instance FromJSON TaskPriority

derivePersistField "TaskStatus"
derivePersistField "TaskPriority"

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    username String
    role String
    createdAt UTCTime
    deriving Show Generic

Task sql=tasks
    name String sql=task_name
    userId Int
    createdAt UTCTime
    status TaskStatus default='Pending'
    dueDate Day
    priority TaskPriority default='Medium'
    deriving Show Generic
|]

instance ToJSON Task
instance FromJSON Task

-- data TaskWithId = TaskWithId
--   { taskId :: Int64
--   , taskName :: String
--   , taskUserId :: Int
--   , taskCreatedAt :: UTCTime
--   , taskDueDate :: Day
--   , taskStatus :: TaskStatus
--   , taskPriority :: TaskPriority
--   } deriving (Generic)
--
-- instance ToJSON TaskWithId

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

-- crud
-- endpoints:
-- GET /api/tasks?user_id=2137 -> [Task]
-- POST /api/tasks -> Task
-- PUT /api/tasks/<task_id> -> Task
-- PATCH /api/tasks/<task_id>/complete -> Nothing
-- DELETE /api/tasks/<task_id> -> Nothing

-- TODO:
-- Error handling przy queryParam i pathParam type cast
--

main :: IO ()
main = runStdoutLoggingT $ withPostgresqlPool connStr 3 $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ runMigration migrateAll

    liftIO $ scotty 3000 $ do
        S.get "/api/tasks" $ do
            uid <- queryParam "user_id" :: S.ActionM Int
            tasks <- liftIO $ flip runSqlPersistMPool pool $ selectList [TaskUserId ==. uid] []
            S.status status200
            S.json $ map (\(Entity tid t) -> object
                [ "id" .= fromSqlKey tid
                , "taskName" .= taskName t
                , "taskUserId" .= taskUserId t
                , "taskCreatedAt" .= taskCreatedAt t
                , "taskDueDate" .= taskDueDate t
                , "taskStatus" .= taskStatus t
                , "taskPriority" .= taskPriority t
                ]) tasks


        S.post "/api/tasks/" $ do
            task <- S.jsonData :: S.ActionM Task
            taskKey <- liftIO $ (flip runSqlPersistMPool pool $ insert task :: IO (Key Task))
            let taskIdNum = fromSqlKey taskKey :: Int64
            S.status status204
            S.json $ object ["id" .= taskIdNum]

        S.patch "/api/tasks/:task_id/complete" $ do
            taskId <- pathParam "task_id" :: S.ActionM Int
            let taskKey = toSqlKey (fromIntegral taskId) :: Key Task
            liftIO $ flip runSqlPersistMPool pool $ updateWhere [TaskId ==. taskKey] [TaskStatus =. Completed]
            S.status status204
            return ()

        S.put "/api/task/:task_id" $ do
            taskId <- pathParam "task_id" :: S.ActionM Int
            S.text $ T.pack ("PUT /api/tasks/" ++ show taskId)

        S.delete "/api/tasks/:task_id" $ do
            taskId <- pathParam "task_id" :: S.ActionM Int
            S.text $ T.pack ("DELETE /api/tasks/" ++ show taskId)




