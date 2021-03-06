{-# LANGUAGE UndecidableInstances #-}
module Base.Database where

import           Boots
import           Boots.Web
import           Control.Exception           (Exception, finally, throw)
import           Control.Monad.IO.Unlift
import           Control.Monad.Reader
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as B
import           Data.Maybe
import           Data.Pool
import qualified Data.Text                   as T
import           Data.Word
import           Database.Persist.Postgresql
import           Database.Persist.Sqlite
import           Salak


data DatabaseException = DatabaseNotInitializedException deriving Show

instance Exception DatabaseException

data DBType = PostgreSQL | SQLite deriving Show

instance FromProp m DBType where
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

buildDatabase
  :: (HasSalak env, HasLogger env, HasHealth env, MonadMask m, MonadUnliftIO m)
  => Factory m env DB
buildDatabase = do
  enabled <- fromMaybe True <$> require "datasource.enabled"
  if enabled
    then do
      dbtype <- fromMaybe SQLite <$> require "datasource.type"
      logInfo $ "Load database " <> toLogStr (show dbtype)
      db     <- case dbtype of
        PostgreSQL -> require "datasource.postgresql" >>= buildPostresql
        _          -> require "datasource.sqlite"     >>= buildSqlite
      return db
    else do
      logInfo "Disable database module"
      return $ throw DatabaseNotInitializedException

data DBE = DBE
  { dbelog :: !Boots.LogFunc
  , dbepoo :: !(Pool SqlBackend)
  }

instance HasLogger DBE where
  askLogger = lens dbelog (\x y -> x {dbelog = y})

instance HasDataSource DBE where
  askDataSource = lens (DB . dbepoo) (\x y -> x {dbepoo = dbConn y})

instance HasDataSource ext => HasDataSource (AppEnv ext) where
  askDataSource = askExt . askDataSource
instance HasWeb context DB => HasDataSource (Context context) where
  askDataSource = askWeb . askDataSource

check :: Boots.LogFunc -> Pool SqlBackend -> IO HealthStatus
check l p = runAppT (DBE l p) (runTrans $ return UP)

runTrans :: (HasLogger env, HasDataSource env) => SqlPersistT (App env) a -> App env a
runTrans ma = do
  env  <- ask
  let DB{..} = view askDataSource env
      logF   = view askLogger     env
  (a,b)  <- liftIO $ takeResource dbConn
  liftIO $ runAppT env (runReaderT ma a { connLogFunc = toMonadLogger logF }) `finally` destroyResource dbConn b a

data PGConfig = PGConfig
  { host     :: !ByteString
  , port     :: !Word16
  , user     :: !ByteString
  , password :: !(Maybe ByteString)
  , dbname   :: !ByteString
  , maxConns :: !Word16
  }

instance FromProp m PGConfig where
  fromProp = PGConfig
    <$> "host"      .?= "localhost"
    <*> "port"      .?= 5432
    <*> "user"      .?= "postgres"
    <*> "password"
    <*> "dbname"    .?= "postgres"
    <*> "max-conns" .?= 10


buildPostresql
  :: (HasLogger env, HasHealth env, MonadMask m, MonadUnliftIO m)
  => PGConfig -> Factory m env DB
buildPostresql PGConfig{..} = do
  lfc <- asksEnv (view askLogger)
  let go = B.intercalate " "
        [ "host=" <> host
        , "port=" <> fromString (show port)
        , "user=" <> user
        , maybe "" ("password=" <>) password
        , "dbname=" <> dbname
        ]
  conn <- natTrans (`runLoggingT` (toMonadLogger lfc)) lift
    $ wrap
    $ withPostgresqlPool go (fromIntegral maxConns)
  registerHealth "PostgreSQL" (check lfc conn)
  return (DB conn)


data SQLiteConfig = SQLiteConfig
  { connStr  :: !T.Text
  , maxConns :: !Word16
  }

instance FromProp m SQLiteConfig where
  fromProp = SQLiteConfig
    <$> "conn"      .?= ":memory:"
    <*> "max-conns" .?= 1

buildSqlite
  :: (HasLogger env, HasHealth env, MonadMask m, MonadUnliftIO m)
  => SQLiteConfig -> Factory m env DB
buildSqlite SQLiteConfig{..} = do
  lfc  <- asksEnv (view askLogger)
  conn <- natTrans (`runLoggingT` (toMonadLogger lfc)) lift
    $ wrap
    $ withSqlitePool connStr (fromIntegral maxConns)
  registerHealth "SQLite" (check lfc conn)
  return (DB conn)

