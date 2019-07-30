module Base.Main where

import           Base.Database
import           Base.Web
import           Boots
import           Data.Default
import           Data.Proxy
import           Data.Version
import           Lens.Micro
import           Network.Wai
import           Salak
import           Servant

data WebEnv = WebEnv
  { salak      :: SourcePack
  , logfunc    :: LogFunc
  , config     :: WebConfig
  , health     :: IO Health
  , middleware :: Middleware
  , swagger    :: SwaggerProxy
  , serveWeb   :: ServeWeb '[WebEnv]
  , natureT    :: NatureT WebEnv (App WebEnv)


  -- Db
  , database   :: DB
  }

class HasWebEnv env where
  askWebEnv :: Lens' env WebEnv

instance HasWebEnv WebEnv where
  askWebEnv = id

instance HasServeWeb '[WebEnv] WebEnv where
  askServeWeb = lens serveWeb (\x y -> x { serveWeb = y})

instance HasWebConfig WebEnv where
  askWebConfig = lens config (\x y -> x { config = y})

instance HasSalak WebEnv where
  askSourcePack = lens salak (\x y -> x { salak = y})

instance HasLogger WebEnv where
  askLogger = lens logfunc (\x y -> x { logfunc = y})

instance HasSwaggerProxy WebEnv where
  askSwaggerProxy = lens swagger (\x y -> x { swagger = y})

instance HasHealth WebEnv where
  askHealth = lens health (\x y -> x { health = y})

instance HasMiddleware WebEnv where
  askMiddleware = lens middleware (\x y -> x { middleware = y})

instance HasNatureT WebEnv (App WebEnv) WebEnv where
  askNT = lens natureT (\x y -> x { natureT = y})

instance Default (NatureT WebEnv (App WebEnv)) where
  def = NatureT $ \_ _ e -> liftIO . runAppT e

start
  :: forall api
  .( HasSwagger api, HasServer api '[WebEnv])
  => Version
  -> Proxy api
  -> ServerT api (App WebEnv)
  -> IO ()
start v proxy server = boot $ do
  salak   <- pluginSalak "application"
  promote salak $ do
    config  <- require "application"
    logfunc <- pluginLogger (name config)
    db      <- promote (Simple salak logfunc) pluginDatabase
    promote (WebEnv salak logfunc config emptyHealth id def def def db)
      $ pluginWeb v (Proxy @(App WebEnv)) proxy server

type DemoAPI = "hello" :> Get '[PlainText] String

startDemo = start (makeVersion []) (Proxy @DemoAPI) (logInfo "hello" >> return "Hello")

