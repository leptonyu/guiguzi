module Base.Web.Types where

import           Base.Dto
import           Base.Health
import           Base.Metrics
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
import           Data.Version
import           Data.Word
import           GHC.TypeLits
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
  , health :: !(IO Health)
  , store  :: !Store
  , nature :: forall a. Proxy cxt -> Proxy m -> Vault -> m a -> Servant.Handler a
  , middle :: !Middleware
  , serveW :: forall api. HasServer api '[cxt] => Proxy api -> Context '[cxt] -> Server api -> Application
  , swagge :: forall api. HasSwagger api => Proxy api -> Swagger
  }

class HasWeb m cxt env where
  askWeb :: Lens' env (Web m cxt)

instance HasWeb m cxt (Web m cxt) where
  askWeb = id

askContext :: Lens' (Web n cxt) cxt
askContext = lens context (\x y -> x { context = y })
instance HasLogger cxt => HasLogger (Web m cxt) where
  askLogger = askContext . askLogger
instance HasSalak cxt => HasSalak (Web m cxt) where
  askSourcePack = askContext . askSourcePack
instance HasApp cxt => HasApp (Web m cxt) where
  askApp = askContext . askApp
instance HasHealth (Web m cxt) where
  askHealth = lens health (\x y -> x { health = y })
instance HasMetrics (Web m cxt) where
  askMetrics = lens store (\x y -> x { store = y })

defWeb :: cxt -> Store -> WebConfig -> Web Servant.Handler cxt
defWeb cxt s wc = Web cxt wc emptyHealth s (\_ _ _ -> id) id serveWithContext toSwagger

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

buildWeb
  :: forall m cxt n env
  . ( MonadThrow n
    , MonadIO n
    , HasApp cxt
    , HasWeb m cxt env
    , HasLogger env
    , HasSalak env)
  => Plugin env n (IO ())
buildWeb = do
  (Web{..} :: Web m cxt) <- asks (view askWeb)
  let AppContext{..} = view askApp context
  SwaggerConfig{..}      <- require "swagger"
  let portText = fromString (show $ port config)
  when enabled $
    logInfo  $ "Swagger enabled: http://"<> fromString (hostname config) <> ":" <> portText <> "/" <> pack urlDir
  logInfo $ "Service started on port(s): " <> portText
  let proxy = Proxy @EmptyAPI
  return
    $ serveWarp config
    $ middle
    $ if enabled
        then reifySymbol urlDir
          $ \pd -> reifySymbol urlSchema
          $ \ps -> serveW (gos pd ps) (context :. EmptyContext) (swaggerSchemaUIServer
            $ baseInfo (hostname config) name ver (fromIntegral $ port config)
            $ swagge proxy)
        else serveW proxy (context :. EmptyContext) emptyServer
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
  => Proxy m
  -> Proxy cxt
  -> Bool
  -> Proxy api
  -> ServerT api m
  -> Plugin env n env
serveWeb pm pcxt b proxy server =
  if b
    then asks
      $ over askWeb
      $ \(web :: Web m cxt) -> web { serveW =
        \p c s -> serveW web (gop p proxy) c
          $ s :<|> (\v -> hoistServerWithContext proxy (Proxy @'[cxt]) (nature web pcxt pm v) server) }
    else ask

gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> (Vault :> b))
gop _ _ = Proxy


serveWebWithSwagger
  :: forall cxt n m env api
  . ( HasServer api '[cxt]
    , HasSwagger api
    , HasWeb m cxt env)
  => Proxy m
  -> Proxy cxt
  -> Bool
  -> Proxy api
  -> ServerT api m
  -> Plugin env n env
serveWebWithSwagger pm pcxt b proxy server = combine
  [ serveWeb pm pcxt b proxy server
  , if b then go else ask
  ]
  where
    go = asks
      $ over askWeb
      $ \(web :: Web m cxt) -> web { swagge = \p -> swagge web (gop p proxy) }


middlewarePlugin :: forall m cxt env n. HasWeb m cxt env => Proxy m -> Proxy cxt -> Middleware -> Plugin env n env
middlewarePlugin _ _ md = asks $ over askWeb $ \(web :: Web m cxt) -> web { middle = md . middle web }


-- | Swagger modification
baseInfo
  :: String  -- ^ Hostname
  -> Text    -- ^ Server Name
  -> Version -- ^ Server version
  -> Int     -- ^ Port
  -> Swagger -- ^ Old swagger
  -> Swagger
baseInfo hostName n v p s = s
  & info . title   .~ (n <> " API Documents")
  & info . version .~ pack (showVersion v)
  & host ?~ Host hostName (Just $ fromIntegral p)

data SwaggerTag (name :: Symbol) (desp :: Symbol)

instance HasServer api ctx
  => HasServer (SwaggerTag name desp :> api) ctx where
  type ServerT (SwaggerTag name desp :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

-- instance HasClient m api
--   => HasClient m (SwaggerTag name desp :> api) where
--   type  Client m (SwaggerTag name desp :> api) = Client m api
--   clientWithRoute _ _ = clientWithRoute (Proxy @m) (Proxy @api)
--   hoistClientMonad pm _ = hoistClientMonad pm (Proxy @api)

instance (HasSwagger api, KnownSymbol name, KnownSymbol desp)
  => HasSwagger (SwaggerTag name desp :> api) where
  toSwagger _ = toSwagger (Proxy @api) & applyTags [tag]
    where
      tag = Tag (go (Proxy @name)) (g2 $ go (Proxy @desp)) Nothing
      go :: forall a. KnownSymbol a => Proxy a -> Text
      go  = pack . symbolVal
      g2 "" = Nothing
      g2 a  = Just a

