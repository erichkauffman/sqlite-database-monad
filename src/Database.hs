{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Database
  ( Database,
    DatabaseT,
    DbConnection (..),
    DbEither,
    dbT,
    dbEither,
    iodb,
    read,
    readWithParams,
    readWithParams_,
    runDatabaseT,
    runIO,
    runLiftIO,
    write,
    writeMultiple,
    writeWithId,
  )
where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Newtype (Newtype, pack)
import Data.Aeson.Types (FromJSON)
import Data.Text (Text)
import Database.SQLite.Simple (Connection, FromRow, NamedParam, Query (..), ToRow)
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

runLiftIO :: (MonadIO m) => DbConnection -> Database a -> m a
runLiftIO dbConnection = liftIO . runIO dbConnection

createDb :: (Connection -> IO a) -> Database a
createDb = Database . ReaderT

iodb :: IO a -> Database a
iodb io = createDb $ const io

read :: (FromRow a) => Text -> Database [a]
read query =
  createDb (\dbConnection -> Simple.query_ dbConnection $ Query query)

readWithParams ::
  (FromRow a) =>
  Text ->
  [NamedParam] ->
  Database [a]
readWithParams query params =
  createDb (\dbConnection -> Simple.queryNamed dbConnection (Query query) params)

readWithParams_ ::
  (FromRow a, ToRow q) =>
  Text ->
  q ->
  Database [a]
readWithParams_ query params =
  createDb (\dbConnection -> Simple.query dbConnection (Query query) params)

write :: Text -> [NamedParam] -> Database ()
write query params =
  createDb (\dbConnection -> Simple.executeNamed dbConnection (Query query) params)

writeWithId :: (Newtype a b, Integral b) => Text -> [NamedParam] -> Database a
writeWithId query params =
  createDb
    ( \dbConnection -> do
        Simple.executeNamed dbConnection (Query query) params
        pack . fromIntegral <$> Simple.lastInsertRowId dbConnection
    )

writeMultiple :: (ToRow q) => Text -> [q] -> Database ()
writeMultiple query params =
  createDb (\dbConnection -> Simple.executeMany dbConnection (Query query) params)

newtype DatabaseT m a = DatabaseT (Database (m a))

instance (Functor m) => Functor (DatabaseT m) where
  fmap :: (a -> b) -> DatabaseT m a -> DatabaseT m b
  fmap f (DatabaseT dbMA) = DatabaseT $ fmap (fmap f) dbMA

instance (Applicative m) => Applicative (DatabaseT m) where
  pure :: a -> DatabaseT m a
  pure a = DatabaseT $ pure $ pure a

  (<*>) :: DatabaseT m (a -> b) -> DatabaseT m a -> DatabaseT m b
  (DatabaseT dbMAtoB) <*> (DatabaseT dbMA) =
    DatabaseT $
      liftA2
        (<*>)
        dbMAtoB
        dbMA

instance (Monad m, Traversable m) => Monad (DatabaseT m) where
  (>>=) :: DatabaseT m a -> (a -> DatabaseT m b) -> DatabaseT m b
  (DatabaseT dbMA) >>= f = DatabaseT $ do
    mA <- dbMA
    let mDbTMB = fmap f mA
    let mDbMB = fmap (\(DatabaseT dbMB) -> dbMB) mDbTMB
    join <$> sequence mDbMB

dbT :: Database (m a) -> DatabaseT m a
dbT = DatabaseT

type DbEither e a = DatabaseT (Either e) a

dbEither :: Database (Either e a) -> DbEither e a
dbEither = dbT

runDatabaseT :: DatabaseT m a -> Database (m a)
runDatabaseT (DatabaseT dbma) = dbma
