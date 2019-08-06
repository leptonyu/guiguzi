module Base.Env where

import           Base.Client
import           Base.Health
import           Boots
import           Lens.Micro

data MainEnv db = MainEnv
  { app    :: !AppEnv
  , health :: !(IO Health)
  -- Client
  , client :: !HttpClient
  -- dbs
  , db     :: !db
  }

data EmptyDB = EmptyDB

class HasMainEnv db env where
  askMainEnv :: Lens' env (MainEnv db)
instance HasMainEnv db (MainEnv db) where
  askMainEnv = id
instance HasSalak (MainEnv db) where
  askSourcePack = askApp . askSourcePack
instance HasLogger (MainEnv db) where
  askLogger = askApp . askLogger
instance HasHealth (MainEnv db) where
  askHealth = lens health (\x y -> x { health = y})
instance HasApp (MainEnv db) where
  askApp = lens app (\x y -> x { app = y})
instance HasHttpClient (MainEnv db) where
  askHttpClient = lens client (\x y -> x { client = y})

askDb :: Lens' (MainEnv db) db
askDb = lens db (\x y -> x { db = y })
