module Base.Database where

import           Boots
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.Maybe
import           Data.Monoid
import           Data.Pool
import           Data.String
import qualified Data.Text                      as T
import           Data.Word
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Lens.Micro
import           Salak

data DBType = PostgreSQL | SQLite deriving Show

instance Monad m => FromProp m DBType where
  fromProp = readEnum (go . T.toLower)
    where
      go "postgresql" = Right PostgreSQL
      go "pg"         = Right PostgreSQL
      go "sqlite"     = Right SQLite
      go k            = Left $ "dbtype " ++ T.unpack k ++ " invalid"

type DB = Pool SqlBackend

class HasDataSource env where
  askDataSource :: Lens' env DB

instance HasDataSource DB where
  askDataSource = id

pluginDatabase :: (HasSalak env, HasLogger env, MonadThrow m, MonadUnliftIO m) => Plugin env m DB
pluginDatabase = do
  dbtype <- fromMaybe SQLite <$> require "datasource.type"
  logInfo $ "Loading database " <> T.pack (show dbtype)
  case dbtype of
    PostgreSQL -> require "datasource.postgresql" >>= pluginPostresql
    _          -> require "datasource.sqlite"     >>= pluginSqlite

data PGConfig = PGConfig
  { host     :: ByteString
  , port     :: Word16
  , user     :: ByteString
  , password :: ByteString
  , dbname   :: ByteString
  , maxConns :: Word16
  }

instance Monad m => FromProp m PGConfig where
  fromProp = PGConfig
    <$> "host"      .?= "localhost"
    <*> "port"      .?= 5432
    <*> "user"      .?= "postgres"
    <*> "password"
    <*> "dbname"    .?= "postgres"
    <*> "max-conns" .?= 10


pluginPostresql :: (HasLogger env, MonadThrow m, MonadUnliftIO m) => PGConfig -> Plugin env m DB
pluginPostresql PGConfig{..} = do
  lf <- askLoggerIO
  let go = B.intercalate " " $
        [ "host=" <> host
        , "port=" <> fromString (show port)
        , "user=" <> user
        , "password=" <> password
        , "dbname=" <> dbname
        ]
  isoPlugin lift (`runLoggingT` lf)
    $ wrapP
    $ withPostgresqlPool go (fromIntegral maxConns)


data SQLiteConfig = SQLiteConfig
  { connStr  :: T.Text
  , maxConns :: Word16
  }

instance Monad m => FromProp m SQLiteConfig where
  fromProp = SQLiteConfig
    <$> "conn"      .?= ":memory:"
    <*> "max-conns" .?= 1

pluginSqlite :: (HasLogger env, MonadThrow m, MonadUnliftIO m) => SQLiteConfig -> Plugin env m DB
pluginSqlite SQLiteConfig{..} = do
  lf <- askLoggerIO
  isoPlugin lift (`runLoggingT` lf)
    $ wrapP
    $ withSqlitePool connStr (fromIntegral maxConns)

