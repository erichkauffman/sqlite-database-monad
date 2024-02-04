{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Database
  ( Database,
    DbConnection (..),
    read,
    readWithParams,
    runIO,
    runLiftIO,
    write,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson.Types (FromJSON)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Query (..))
import qualified Database.SQLite.Simple as Simple
import GHC.Generics (Generic)
import Prelude hiding (read)

newtype DbConnection = DbConnection String deriving (Generic)

instance FromJSON DbConnection

newtype Database a = Database (ReaderT Connection IO a)

instance Functor Database where
  fmap :: (a -> b) -> Database a -> Database b
  fmap f (Database dbReader) = Database $ fmap f dbReader

instance Applicative Database where
  pure :: a -> Database a
  pure a = Database $ pure a

  (<*>) :: Database (a -> b) -> Database a -> Database b
  (Database dbReaderAtoB) <*> (Database dbReaderA) =
    Database $ dbReaderAtoB <*> dbReaderA

instance Monad Database where
  (>>=) :: Database a -> (a -> Database b) -> Database b
  (Database dbReaderA) >>= f = Database $ do
    a <- dbReaderA
    let (Database dbReaderB) = f a
    dbReaderB

runIO :: DbConnection -> Database a -> IO a
runIO (DbConnection dbFile) (Database dbReader) = do
  dbConnection <- Simple.open dbFile
  result <- Simple.withTransaction dbConnection $ runReaderT dbReader dbConnection
  Simple.close dbConnection
  return result

runLiftIO :: MonadIO m => DbConnection -> Database a -> m a
runLiftIO dbConnection = liftIO . runIO dbConnection

createDb :: (Connection -> IO a) -> Database a
createDb = Database . ReaderT

read :: FromRow a => Text -> Database [a]
read query =
  createDb (\dbConnection -> Simple.query_ dbConnection $ Query query)

readWithParams :: FromRow a => Text -> [NamedParam] -> Database [a]
readWithParams query params =
  createDb (\dbConnection -> Simple.queryNamed dbConnection (Query query) params)

write :: Text -> [NamedParam] -> Database ()
write query params =
  createDb (\dbConnection -> Simple.executeNamed dbConnection (Query query) params)
