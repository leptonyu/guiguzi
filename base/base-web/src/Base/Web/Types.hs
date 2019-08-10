module Base.Web.Types where

import           Base.Health
import           Base.Metrics
import           Base.Web.Swagger
import           Boots
import           Control.Exception
    ( Exception (..)
    , SomeException
    )
import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.Reflection
import           Data.String
import           Data.Swagger                        (Swagger)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Salak
import           Servant
import           Servant.Server.Internal.ServerError (responseServerError)
import           Servant.Swagger
import           Servant.Swagger.UI.JensOleG

-- | Application Configuration.
data WebConfig = WebConfig
  { hostname :: !String -- ^ Applicatoin hostname, used in swagger.
  , port     :: !Word16 -- ^ Application http port.
  } deriving (Eq, Show)

instance Default WebConfig where
  def = WebConfig "localhost" 8888

instance Monad m => FromProp m WebConfig where
  fromProp = WebConfig
    <$> "host" .?: hostname
    <*> "port" .?: port

data Web m cxt = Web
  { context:: !cxt
  , config :: !WebConfig
  , store  :: !Store
  , middle :: !Middleware
  , nature :: forall a. Vault -> m a -> IO a
  , serveW :: forall api. HasServer api '[cxt] => Proxy api -> Context '[cxt] -> Server api -> Application
  , swagge :: forall api. HasSwagger api => Proxy api -> Swagger
  }

class HasWeb m cxt env | env -> cxt m where
  askWeb :: Lens' env (Web m cxt)

instance HasWeb m cxt (Web m cxt) where
  askWeb = id

askContext :: Lens' (Web n cxt) cxt
askContext = lens context (\x y -> x { context = y })
instance HasApp cxt cxt => HasApp cxt (Web m cxt) where
  askApp = askContext . askApp
instance HasVault cxt cxt => HasVault cxt (Web m cxt) where
  askVault = askContext . askVault
instance HasHealth cxt => HasHealth (Web m cxt) where
  askHealth = askContext . askHealth
instance HasLogger cxt => HasLogger (Web m cxt) where
  askLogger = askContext . askLogger
instance HasSalak cxt => HasSalak (Web m cxt) where
  askSourcePack = askContext . askSourcePack
instance HasMetrics (Web m cxt) where
  askMetrics = lens store (\x y -> x { store = y })

defWeb :: cxt -> Store -> WebConfig -> Web IO cxt
defWeb cxt s wc = Web cxt wc s id (\_ -> id) serveWithContext toSwagger

runWeb
  ::( HasWeb m cxt env
    , MonadCatch n
    , MonadIO n
    , HasApp cxt cxt
    , HasLogger env
    , HasSalak env)
  => Factory n env (IO ())
runWeb = do
  Web{..}<- asks (view askWeb)
  let AppEnv{..} = view askApp context
      portText = fromString (show $ port config)
  SwaggerConfig{..}      <- require "swagger"
  when enabled $ do
    logInfo  $ "Swagger enabled: http://"<> fromString (hostname config) <> ":" <> portText <> "/" <> fromString urlDir
  logInfo $ "Service started on port(s): " <> portText
  delayA $ logInfo "Service ended"
  return
    $ serveWarp config
    $ middle
    $ if enabled
      then reifySymbol urlDir
            $ \pd -> reifySymbol urlSchema
            $ \ps -> serveW (gos pd ps) (context :. EmptyContext) (jensolegSwaggerSchemaUIServer
              $ baseInfo (hostname config) name version (fromIntegral $ port config)
              $ swagge (Proxy @EmptyAPI))
      else serveW (Proxy @EmptyAPI) (context :. EmptyContext) emptyServer
  where
    gos :: forall a b. Proxy a -> Proxy b -> Proxy (SwaggerSchemaUI a b)
    gos _ _ = Proxy
    whenException :: SomeException -> Network.Wai.Response
    whenException e = responseServerError
      $ fromMaybe err400 { errBody = fromString $ show e} (fromException e :: Maybe ServerError)
    serveWarp WebConfig{..} = runSettings
      $ defaultSettings
      & setPort (fromIntegral port)
      & setOnExceptionResponse whenException


serveWebWithSwagger
  ::( HasServer api '[cxt]
    , HasSwagger api
    , HasWeb m cxt env)
  => Bool
  -> Proxy api
  -> ServerT api m
  -> Factory n env env
serveWebWithSwagger b proxy server = mconcat
  [ serveWeb     b proxy server
  , serveSwagger b proxy
  ]

serveSwagger
  ::( HasWeb m cxt env
    , HasSwagger api)
  => Bool -> Proxy api -> Factory n env env
serveSwagger b proxy = tryBuild b
  $ asks
  $ over askWeb
  $ \web -> web { swagge = \p -> swagge web (gop p proxy) }

serveWeb
  :: forall m cxt env api n
  . ( HasServer api '[cxt]
    , HasWeb m cxt env)
  => Bool
  -> Proxy api
  -> ServerT api m
  -> Factory n env env
serveWeb b proxy server = tryBuild b
  $ asks
  $ over askWeb
  $ \web -> web { serveW =
    \p c s -> serveW web (gop p proxy) c
      $ s :<|> (\v -> hoistServerWithContext proxy (Proxy @'[cxt]) (go . nature web v) server) }
  where
    go :: IO a -> Servant.Handler a
    go = liftIO

gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> (Vault :> b))
gop _ _ = Proxy

tryBuild :: Bool -> Factory n env env -> Factory n env env
tryBuild b p = if b then p else ask

buildMiddleware :: HasWeb m cxt env => Middleware -> Factory n env env
buildMiddleware md = asks $ over askWeb $ \web -> web { middle = md . middle web }


