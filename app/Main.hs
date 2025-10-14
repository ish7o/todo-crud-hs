{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Web.Scotty
import qualified Web.Scotty as S
import Network.HTTP.Types.Status
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Database.Persist.Postgresql (runMigration)
import Data.Aeson (object, (.=))

import Db (createPool, runDb)
import Types (migrateAll, Task(..))
import Service

main :: IO ()
main = do
  pool <- createPool
  runDb pool $ runMigration migrateAll

  scotty 3000 $ do
    S.get "/api/tasks" $ do
      uid <- S.queryParam "user_id" :: ActionM Int
      tasks <- liftIO $ runDb pool $ getTasksForUserService uid
      S.json tasks

    S.post "/api/tasks" $ do
      task <- S.jsonData :: ActionM Task
      newId <- liftIO $ runDb pool $ createTaskService task
      S.json $ object ["id" .= newId]

    S.patch "/api/tasks/:id/complete" $ do
      tid <- S.pathParam "id" :: ActionM Int
      liftIO $ runDb pool $ completeTaskService tid
      S.status status204

    S.put "/api/tasks/:id" $ do
      tid <- S.pathParam "id" :: ActionM Int
      task <- S.jsonData :: ActionM Task
      updated <- liftIO $ runDb pool $ updateTaskService tid task
      if updated
        then S.status status200
        else S.status status404 >> S.text "Task not found"

    S.delete "/api/tasks/:id" $ do
      tid <- S.pathParam "id" :: ActionM Int
      deleted <- liftIO $ runDb pool $ deleteTaskService tid
      if deleted
        then S.status status200
        else S.status status404 >> S.text "Task not found"

