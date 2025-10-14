{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

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

getTasksForUserService :: Int -> SqlPersistM [TaskDTO]
getTasksForUserService uid = do
  tasks <- getTasksForUser uid
  return $ map entityToDTO tasks

createTaskService :: T.Task -> SqlPersistM Int64
createTaskService task = fromSqlKey <$> insertTask task

completeTaskService :: Int -> SqlPersistM ()
completeTaskService tid = completeTask (toSqlKey (fromIntegral tid))

updateTaskService :: Int -> T.Task -> SqlPersistM Bool
updateTaskService tid newTask = do
  let key = toSqlKey (fromIntegral tid)
  maybeTask <- getTaskById key
  case maybeTask of
    Nothing -> return False
    Just _  -> updateTask key newTask >> return True

deleteTaskService :: Int -> SqlPersistM Bool
deleteTaskService tid = deleteTask (toSqlKey $ fromIntegral tid)

