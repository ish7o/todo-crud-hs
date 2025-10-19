{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Service where

import Database.Persist
import Database.Persist.Sql (SqlPersistM, fromSqlKey, toSqlKey)
import Data.Int (Int64)
import Data.Time (UTCTime, Day)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Repository
import qualified Types as T

data TaskDTO = TaskDTO
  { taskId        :: Int
  , taskName      :: String
  , taskUserId    :: Int
  , taskCreatedAt :: UTCTime
  , taskDueDate   :: Day
  , taskStatus    :: T.TaskStatus
  , taskPriority  :: T.TaskPriority
  } deriving (Show, Generic)

instance ToJSON TaskDTO
instance FromJSON TaskDTO

entityToDTO :: Entity T.Task -> TaskDTO
entityToDTO (Entity tid t) = TaskDTO
  { taskId        = fromIntegral (fromSqlKey tid)
  , taskName      = T.taskName t
  , taskUserId    = T.taskUserId t
  , taskCreatedAt = T.taskCreatedAt t
  , taskDueDate   = T.taskDueDate t
  , taskStatus    = T.taskStatus t
  , taskPriority  = T.taskPriority t
  }

class TaskRepository m => TaskService m where
    getTasksForUserService :: Int -> m [TaskDTO]
    createTaskService :: T.Task -> m Int64
    completeTaskService :: Int -> m ()
    updateTaskService ::  Int -> T.Task -> m Bool
    deleteTaskService ::  Int -> m Bool

instance TaskService SqlPersistM where
    getTasksForUserService uid = do
      tasks <- getTasksForUser uid
      return $ map entityToDTO tasks

    createTaskService task = fromSqlKey <$> insertTask task

    completeTaskService tid = completeTask (toSqlKey $ fromIntegral tid)

    updateTaskService tid newTask = do
      let key = toSqlKey (fromIntegral tid)
      maybeTask <- getTaskById key
      case maybeTask of
        Nothing -> return False
        Just _  -> updateTask key newTask >> return True

    deleteTaskService tid = deleteTask (toSqlKey $ fromIntegral tid)

