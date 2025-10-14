{-# LANGUAGE OverloadedStrings #-}

module Db (createPool, runDb) where

import Database.Persist.Postgresql
import Control.Monad.Logger (runStdoutLoggingT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Pool (Pool)

createPool :: IO (Pool SqlBackend)
createPool = runStdoutLoggingT $
  createPostgresqlPool connStr 3

connStr :: ConnectionString
connStr = "host=localhost port=5432 user=postgres password=12345 dbname=db"

runDb :: Pool SqlBackend -> SqlPersistM a -> IO a
runDb pool query = liftIO $ runSqlPersistMPool query pool

