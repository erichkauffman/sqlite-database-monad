{-# LANGUAGE InstanceSigs #-}

module Database
  ( Database,
    read,
    runIO,
    runLiftIO,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, Query (..))
import qualified Database.SQLite.Simple as Simple
import Prelude hiding (read)

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

runIO :: String -> Database a -> IO a
runIO dbFile (Database dbReader) = do
  dbConnection <- Simple.open dbFile
  result <- Simple.withTransaction dbConnection $ runReaderT dbReader dbConnection
  Simple.close dbConnection
  return result

runLiftIO :: MonadIO m => String -> Database a -> m a
runLiftIO dbConnection = liftIO . runIO dbConnection

read :: FromRow a => Text -> Database [a]
read query =
  Database $
    ReaderT $
      \dbConnection -> Simple.query_ dbConnection $ Query query
