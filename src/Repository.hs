{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Repository where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Postgresql
import Data.Time (UTCTime, Day)
-- import Types (Task(..), TaskStatus(..), TaskPriority(..), TaskUserId, TaskStatus)
import Types

class Monad m => TaskRepository m where
    getTasksForUser :: Int -> m [Entity Task]
    getTaskById :: Key Task -> m (Maybe Task)
    insertTask :: Task -> m (Key Task)
    updateTask :: Key Task -> Task -> m ()
    completeTask :: Key Task -> m ()
    deleteTask :: Key Task -> m Bool

instance TaskRepository SqlPersistM where
    getTasksForUser uid = selectList [TaskUserId ==. uid] []
    getTaskById = get
    insertTask = insert
    updateTask key newTask = replace key newTask
    completeTask key = update key [TaskStatus =. Completed]
    deleteTask key = do
      existing <- get key
      case existing of
        Nothing -> return False
        Just _  -> delete key >> return True

-- getTasksForUser :: Int -> SqlPersistM [Entity Task]
-- getTasksForUser uid = selectList [TaskUserId ==. uid] []

-- getTaskById :: Key Task -> SqlPersistM (Maybe Task)
-- getTaskById = get

-- insertTask :: Task -> SqlPersistM (Key Task)
-- insertTask = insert

-- updateTask :: Key Task -> Task -> SqlPersistM ()
-- updateTask key newTask = replace key newTask

-- completeTask :: Key Task -> SqlPersistM ()
-- completeTask key = update key [TaskStatus =. Completed]

-- deleteTask :: Key Task -> SqlPersistM Bool
-- deleteTask key = do
--   existing <- get key
--   case existing of
--     Nothing -> return False
--     Just _  -> delete key >> return True

