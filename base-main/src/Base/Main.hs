module Base.Main where

import           Base.Web
import           Boots
import           Control.Exception                   hiding (Handler)
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.String
import           Data.Version
import           Lens.Micro
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Salak
import           Servant
import           Servant.Server.Internal.ServerError (responseServerError)
import           Servant.Swagger

data WebEnv = WebEnv
  { salak      :: SourcePack
  , logfunc    :: LogFunc
  , config     :: WebConfig
  , health     :: IO Health
  , middleware :: Middleware
  , swagger    :: SwaggerProxy
  , serveWeb   :: ServeWeb '[WebEnv]
  , natureT    :: NatureT WebEnv
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

instance HasNatureT WebEnv WebEnv where
  askNT = lens natureT (\x y -> x { natureT = y})

whenException :: SomeException -> Network.Wai.Response
whenException e = responseServerError $ fromMaybe err400 { errBody = fromString $ show e} (fromException e :: Maybe ServerError)

serveWarp :: WebConfig -> Application -> IO ()
serveWarp WebConfig{..} = runSettings
  $ defaultSettings
  & setPort (fromIntegral port)
  & setOnException (\_ _ -> return ())
  & setOnExceptionResponse whenException

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
    let proxycxt     = Proxy @'[WebEnv]
        allActuators :: Proxy (Health :<|> Refresh)
        allActuators = Proxy
    logfunc <- pluginLogger (name config)
    promote (Simple salak logfunc) $ do
      logInfo $ "Start Service [" <> name config <> "] ..."
      env     <- promote (WebEnv salak logfunc config emptyHealth id def def def) $ do
        ac <- require "actuator"
        pluginTrace @WebEnv
          >>= (`promote` actuator proxycxt allActuators ac)
          >>= (`promote` swaggerPlugin v proxycxt proxy)
      logInfo $ "Service started on port(s): " <> fromString (show $ port config)
      let ServeWeb f = serveWeb env
          NatureT nt = natureT env
      return
        $ serveWarp config
        $ middleware env
        $ f (Proxy @(Vault :> api)) (env :. EmptyContext)
        $ \val -> hoistServerWithContext proxy proxycxt (nt val env) server


type DemoAPI = "hello" :> Get '[PlainText] String

startDemo = start (makeVersion []) (Proxy @DemoAPI) (logInfo "hello" >> return "Hello")

