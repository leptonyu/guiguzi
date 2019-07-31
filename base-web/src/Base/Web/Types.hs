module Base.Web.Types where

import           Boots
import           Control.Exception
    ( Exception (..)
    , SomeException
    )
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Default
import           Data.Maybe
import           Data.Proxy
import           Data.Reflection
import           Data.String
import           Data.Swagger                        hiding (name, port)
import           Data.Text                           (Text, pack)
import           Data.Word
import           Lens.Micro
import           Lens.Micro.Extras
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Salak
import           Servant
import           Servant.Server.Internal.ServerError (responseServerError)
import           Servant.Swagger
import           Servant.Swagger.UI

-- | Application Configuration.
data WebConfig = WebConfig
  { name     :: Text   -- ^ Application name.
  , hostname :: String -- ^ Applicatoin hostname, used in swagger.
  , port     :: Word16 -- ^ Application http port.
  } deriving (Eq, Show)

instance Default WebConfig where
  def = WebConfig "application" "localhost" 8888

instance Monad m => FromProp m WebConfig where
  fromProp = WebConfig
    <$> "name" .?: name
    <*> "host" .?: hostname
    <*> "port" .?: port

data Web m cxt = Web
  { config :: WebConfig
  , nature :: forall a. Proxy m -> Vault -> cxt -> m a -> Servant.Handler a
  , middle :: Middleware
  , serveW :: forall api. HasServer api '[cxt] => Proxy api -> Context '[cxt] -> Server api -> Application
  , swagge :: forall api. HasSwagger api => Proxy api -> Swagger
  }

class HasWeb m cxt env where
  askWeb :: Lens' env (Web m cxt)

instance HasWeb m cxt (Web m cxt) where
  askWeb = id

instance Default (Web Servant.Handler cxt) where
  def = Web def (\_ _ _ -> id) id serveWithContext toSwagger

-- ** Swagger
-- | Swagger Configuration
data SwaggerConfig = SwaggerConfig
  { urlDir    :: String -- ^ Url path for swagger.
  , urlSchema :: String -- ^ Api schema path for swagger.
  , enabled   :: Bool   -- ^ If enable swagger.
  } deriving (Eq, Show)

instance Monad m => FromProp m SwaggerConfig where
  fromProp = SwaggerConfig
    <$> "dir"     .?= "swagger-ui"
    <*> "schema"  .?= "swagger-ui.json"
    <*> "enabled" .?= True

webPlugin
  :: forall cxt m n env
  . ( MonadThrow n
    , MonadIO n
    , HasWeb m cxt env
    , HasLogger env
    , HasSalak env)
  => cxt -> Proxy m -> Plugin env n (IO ())
webPlugin cxt _ = do
  (Web{..} :: Web m cxt) <- asks (view askWeb)
  SwaggerConfig{..}      <- require "swagger"
  let portText = fromString (show $ port config)
  when enabled $
    logInfo  $ "Swagger enabled: http://localhost:" <> portText <> "/" <> pack urlDir
  logInfo $ "Service started on port(s): " <> portText
  let proxy = Proxy @EmptyAPI
  return
    $ serveWarp config
    $ middle
    $ if enabled
        then reifySymbol urlDir
          $ \pd -> reifySymbol urlSchema
          $ \ps -> serveW (gos pd ps) (cxt :. EmptyContext) (swaggerSchemaUIServer $ swagge proxy)
        else serveW proxy (cxt :. EmptyContext) emptyServer
  where
    gos :: forall a b. Proxy a -> Proxy b -> Proxy (SwaggerSchemaUI a b)
    gos _ _ = Proxy
    whenException :: SomeException -> Network.Wai.Response
    whenException e = responseServerError
      $ fromMaybe err400 { errBody = fromString $ show e} (fromException e :: Maybe ServerError)
    serveWarp :: WebConfig -> Application -> IO ()
    serveWarp WebConfig{..} = runSettings
      $ defaultSettings
      & setPort (fromIntegral port)
      & setOnException (\_ _ -> return ())
      & setOnExceptionResponse whenException

serveWeb
  :: forall cxt n m env api
  . ( HasServer api '[cxt]
    , HasWeb m cxt env)
  => Bool
  -> cxt
  -> Proxy m
  -> Proxy api
  -> ServerT api m
  -> Plugin env n env
serveWeb b cxt pm proxy server =
  if b
    then asks
      $ over askWeb
      $ \(web :: Web m cxt) -> web { serveW =
        \p c s -> serveW web (gop p proxy) c
          $ s :<|> (\v -> hoistServerWithContext proxy (Proxy @'[cxt]) (nature web pm v cxt) server) }
    else ask

gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> (Vault :> b))
gop _ _ = Proxy


serveWebWithSwagger
  :: forall cxt n m env api
  . ( HasServer api '[cxt]
    , HasSwagger api
    , HasWeb m cxt env)
  => Bool
  -> cxt
  -> Proxy m
  -> Proxy api
  -> ServerT api m
  -> Plugin env n env
serveWebWithSwagger b cxt pm proxy server = combine
  [ serveWeb b cxt pm proxy server
  , if b then go else ask
  ]
  where
    go = asks
      $ over askWeb
      $ \(web :: Web m cxt) -> web { swagge = \p -> swagge web (gop p proxy) }





