module Base.Database where

import           Base.Health
import           Boots
import           Control.Exception              (Exception, finally, throw)
import           Control.Monad.IO.Unlift
import           Control.Monad.Logger.CallStack
import           Control.Monad.Reader
import           Data.ByteString                (ByteString)
import qualified Data.ByteString                as B
import           Data.Maybe
import           Data.Pool
import           Data.String
import qualified Data.Text                      as T
import           Data.Word
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak


data DatabaseException = DatabaseNotInitializedException deriving Show

instance Exception DatabaseException

data DBType = PostgreSQL | SQLite deriving Show

instance Monad m => FromProp m DBType where
  fromProp = readEnum (go . T.toLower)
    where
      go "postgresql" = Right PostgreSQL
      go "pg"         = Right PostgreSQL
      go "sqlite"     = Right SQLite
      go k            = Left $ "dbtype " ++ T.unpack k ++ " invalid"

newtype DB = DB { dbConn :: Pool SqlBackend }

class HasDataSource env where
  askDataSource :: Lens' env DB

instance HasDataSource DB where
  askDataSource = id

pluginDatabase :: (HasSalak env, HasLogger env, MonadThrow m, MonadUnliftIO m) => Plugin env m (DB, Maybe CheckHealth)
pluginDatabase = do
  enabled <- fromMaybe True <$> require "datasource.enabled"
  if enabled
    then do
      dbtype <- fromMaybe SQLite <$> require "datasource.type"
      logInfo $ "Loading database " <> T.pack (show dbtype)
      case dbtype of
        PostgreSQL -> require "datasource.postgresql" >>= pluginPostresql
        _          -> require "datasource.sqlite"     >>= pluginSqlite
    else return (throw DatabaseNotInitializedException, Nothing)

data DBE = DBE
  { dbelog :: Boots.LogFunc
  , dbepoo :: Pool SqlBackend
  }

instance HasLogger DBE where
  askLogger = lens dbelog (\x y -> x {dbelog = y})

instance HasDataSource DBE where
  askDataSource = lens (DB . dbepoo) (\x y -> x {dbepoo = dbConn y})

check :: Boots.LogFunc -> Pool SqlBackend -> IO HealthStatus
check l p = runAppT (DBE l p) (runTrans $ return UP)

runTrans :: (HasLogger env, HasDataSource env) => SqlPersistT (App env) a -> App env a
runTrans ma = do
  lf     <- askLoggerIO
  env    <- ask
  let DB{..} = view askDataSource env
  (a,b)  <- liftIO $ takeResource dbConn
  liftIO $ runAppT env (runReaderT ma a { connLogFunc = lf }) `finally` destroyResource dbConn b a

data PGConfig = PGConfig
  { host     :: ByteString
  , port     :: Word16
  , user     :: ByteString
  , password :: Maybe ByteString
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


pluginPostresql :: (HasLogger env, MonadThrow m, MonadUnliftIO m) => PGConfig -> Plugin env m (DB, Maybe CheckHealth)
pluginPostresql PGConfig{..} = do
  lf  <- askLoggerIO
  lfc <- asks (view askLogger)
  let go = B.intercalate " "
        [ "host=" <> host
        , "port=" <> fromString (show port)
        , "user=" <> user
        , fromMaybe "" $ ("password=" <>) <$> password
        , "dbname=" <> dbname
        ]
  conn <- isoPlugin lift (`runLoggingT` lf)
    $ wrapP
    $ withPostgresqlPool go (fromIntegral maxConns)
  return (DB conn, Just ("PostgreSQL", check lfc conn))


data SQLiteConfig = SQLiteConfig
  { connStr  :: T.Text
  , maxConns :: Word16
  }

instance Monad m => FromProp m SQLiteConfig where
  fromProp = SQLiteConfig
    <$> "conn"      .?= ":memory:"
    <*> "max-conns" .?= 1

pluginSqlite :: (HasLogger env, MonadThrow m, MonadUnliftIO m) => SQLiteConfig -> Plugin env m (DB, Maybe CheckHealth)
pluginSqlite SQLiteConfig{..} = do
  lf   <- askLoggerIO
  lfc  <- asks (view askLogger)
  conn <- isoPlugin lift (`runLoggingT` lf)
    $ wrapP
    $ withSqlitePool connStr (fromIntegral maxConns)
  return (DB conn, Just ("SQLite", check lfc conn))

