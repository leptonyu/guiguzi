module Base.Web.Swagger where

import           Base.Web.Config
import           Base.Web.Serve
import           Boots
import           Control.Monad.Reader
import           Data.Default
import           Data.Proxy
import           Data.Reflection
import           Data.Swagger         hiding (name, port)
import           Data.Text            (Text, pack)
import           Data.Version         (Version, showVersion)
import           GHC.TypeLits
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           Servant
import           Servant.Swagger
import           Servant.Swagger.UI

data SwaggerProxy = SwaggerProxy (forall api. HasSwagger api => Proxy api -> Swagger)

class HasSwaggerProxy env where
  askSwaggerProxy :: Lens' env SwaggerProxy

instance HasSwaggerProxy SwaggerProxy where
  askSwaggerProxy = id

instance Default SwaggerProxy where
  def = SwaggerProxy toSwagger

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


trySwagger :: HasSwagger api => Bool -> Proxy api -> SwaggerProxy -> SwaggerProxy
trySwagger b proxy (SwaggerProxy f) = if b
  then SwaggerProxy $ \p0 -> f (gop p0 proxy)
  else SwaggerProxy f


swaggerPlugin
  :: forall cxt env m api
  . ( HasWebConfig env
    , HasSalak env
    , HasLogger env
    , HasServeWeb cxt env
    , HasSwaggerProxy env
    , HasSwagger api
    , MonadThrow m
    , MonadIO m)
  => Version -> Proxy cxt -> Proxy api -> Plugin env m env
swaggerPlugin v _ proxy = do
  SwaggerConfig{..} <- require "swagger"
  env               <- ask
  let
    WebConfig{..} = view askWebConfig env
    SwaggerProxy f = view askSwaggerProxy env
    go :: ServeWeb cxt -> ServeWeb cxt
    go = reifySymbol urlDir
      $ \pd -> reifySymbol urlSchema
      $ \ps -> tryServeWeb (const id) enabled (gos pd ps)
      $ swaggerSchemaUIServer
      $ baseInfo hostname name v (fromIntegral port)
      $ f proxy
    gos :: forall a b. Proxy a -> Proxy b -> Proxy (SwaggerSchemaUI a b)
    gos _ _ = Proxy
  when enabled $
    logInfo  $ "Swagger enabled: http://localhost:" <> pack (show port) <> "/" <> pack urlDir
  return $ over askServeWeb go env

tryServePlugin
  :: forall cxt env api m n
  . ( HasServer api cxt
    , HasSwagger api
    , HasSwaggerProxy env
    , HasServeWeb cxt env)
  => (Context cxt -> (forall a. m a -> Handler a))
  -> Bool
  -> Proxy cxt
  -> Proxy api
  -> ServerT api m
  -> Plugin env n env
tryServePlugin f b _ proxy server = do
  let
    go :: ServeWeb cxt -> ServeWeb cxt
    go = tryServeWeb f b proxy server
  asks
    $ over askSwaggerProxy (trySwagger b proxy)
    . over askServeWeb     go


-- | Swagger modification
baseInfo
  :: String  -- ^ Hostname
  -> Text    -- ^ Server Name
  -> Version -- ^ Server version
  -> Int     -- ^ Port
  -> Swagger -- ^ Old swagger
  -> Swagger
baseInfo hostName n v p s = s
  & info . title   .~ n
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
