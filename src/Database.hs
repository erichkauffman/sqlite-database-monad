module Database
  ( Database,
    read,
    runIO,
    runLiftIO,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT (..))
import Database.SQLite.Simple (FromRow, Query)
import qualified Database.SQLite.Simple as Simple
import Prelude hiding (read)

newtype Database a = Database (ReaderT String IO a)

instance Functor Database where
  fmap f (Database dbReader) = Database $ fmap f dbReader

instance Applicative Database where
  pure a = Database $ pure a
  (Database dbReaderAtoB) <*> (Database dbReaderA) =
    Database $ dbReaderAtoB <*> dbReaderA

instance Monad Database where
  (Database dbReaderA) >>= f = Database $ do
    a <- dbReaderA
    let (Database dbReaderB) = f a
    dbReaderB

runIO :: String -> Database a -> IO a
runIO dbConnection (Database dbReader) = runReaderT dbReader dbConnection

runLiftIO :: MonadIO m => String -> Database a -> m a
runLiftIO dbConnection = liftIO . runIO dbConnection

read :: FromRow a => Query -> Database [a]
read query = Database $
  ReaderT $ \db -> do
    conn <- Simple.open db
    listA <- Simple.query_ conn query
    Simple.close conn
    return listA
