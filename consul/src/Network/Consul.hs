module Network.Consul where

import           Base.Client
import           Boots
import           Control.Exception   (Exception, throw)
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Proxy
import           Data.Swagger.Schema (ToSchema)
import           Data.Text           (Text, toLower)
import           Data.Word
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.Extras
import           Salak
import           Servant.API
import           Servant.Client

data ServiceKind
  = KindNil
  | KindConnectProxy
  | KindMeshGateway
  deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)

data ServiceConnect = ServiceConnect
  { enabled :: Bool
  } deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)

data ServiceCheck = ServiceCheck
  {

  } deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)

data ServiceWeight = ServiceWeight
  {

  } deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)


data ServiceDef = ServiceDef
  { sname        :: !String
  , sid          :: !(Maybe String)
  , stags        :: ![String]
  , saddr        :: !(Maybe String)
  , saddrmap     :: !(HM.HashMap String String)
  , smeta        :: !(HM.HashMap String String)
  , sport        :: !Word16
  , skind        :: !ServiceKind
  , sconnect     :: !(Maybe ServiceConnect)
  , scheck       :: !(Maybe ServiceCheck)
  , schecks      :: ![ServiceCheck]
  , stagoverride :: !Bool
  , sweights     :: !(Maybe ServiceWeight)
  } deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)



-- class MonadConsul m where
--   registerService ::



type ConsulEndpoint = "v1" :> "agent" :> AgentEndpoint

type AgentEndpoint
  =    "services" :> Get '[JSON] (HM.HashMap Text ServiceDef)
  :<|> "service"  :> Capture "serviceId" Text :> Get '[JSON] ServiceDef
  :<|> "service"  :> "register"    :> ReqBody '[JSON] ServiceDef :> Put '[JSON] NoContent
  :<|> "service"  :> "deregister"  :> Capture "serviceId" Text   :> Put '[JSON] NoContent
  :<|> "service"  :> "maintenance" :> Capture "serviceId" Text   :> Put '[JSON] NoContent
  :<|> "health"   :> "service" :> HealthEndpoint

type HealthEndpoint
  =    "name" :> Capture "serviceName" Text :> Get '[JSON] ServiceDef
  :<|> "id"   :> Capture "serviceId"   Text :> Get '[JSON] ServiceDef

data ConsulApi m env = ConsulApi
  { getServices        :: AppT env m (HM.HashMap Text ServiceDef)
  , getService         :: Text       -> AppT env m ServiceDef
  , registerService    :: ServiceDef -> AppT env m NoContent
  , deregisterService  :: Text       -> AppT env m NoContent
  , maintenanceService :: Text       -> AppT env m NoContent
  , checkHealthByName  :: Text       -> AppT env m ServiceDef
  , checkHealthById    :: Text       -> AppT env m ServiceDef
  }

consulApi :: (HasConsul env, HasHttpClient env, MonadThrow m, MonadIO m) => Proxy m -> Proxy env -> ConsulApi m env
consulApi pm p =
  let getServices
        :<|> getService
        :<|> registerService
        :<|> deregisterService
        :<|> maintenanceService
        :<|> checkHealthByName
        :<|> checkHealthById = hoistClient api (runConsul pm p) (client api)
  in ConsulApi{..}

api :: Proxy ConsulEndpoint
api = Proxy
runConsul
  :: (HasConsul env, HasHttpClient env, MonadThrow m, MonadIO m)
  => Proxy m -> Proxy env -> ClientM a -> AppT env m a
runConsul _ _ cma = do
  env <- ask
  let ConsulConfig bu = view askConsulConfig env
      HttpClient   mg = view askHttpClient   env
  v   <- liftIO $ runClientM cma (ClientEnv mg bu Nothing)
  case v of
    Left  e -> throwM e
    Right a -> return a

consulServer pm p = hoistClient api (runConsul pm p) (client api)

newtype ConsulConfig = ConsulConfig BaseUrl

instance Monad m => FromProp m ConsulConfig where
  fromProp = fmap ConsulConfig $ BaseUrl
    <$> "schema" .?= Http
    <*> "host"   .?= "127.0.0.1"
    <*> "port"   .?= 8500
    <*> "path"   .?= ""

instance Monad m => FromProp m Scheme where
  fromProp = readEnum (go.toLower)
    where
      go "http"  = Right Http
      go "https" = Right Https
      go _       = Left "unkown schema"

class HasConsul env where
  askConsulConfig :: Lens' env ConsulConfig

  askConsul :: forall m. (MonadThrow m, MonadIO m, HasHttpClient env) => ConsulApi m env
  askConsul = consulApi (Proxy @m) (Proxy @env)


instance HasConsul ConsulConfig where
  askConsulConfig = id

data ConsulException = ConsulDisabledException deriving Show
instance Exception ConsulException

pluginConsul :: (HasSalak env, MonadThrow n) => Plugin env n ConsulConfig
pluginConsul = do
  b  <- fromMaybe True <$> require "consul.enabled"
  if b
    then require "consul"
    else return $ throw ConsulDisabledException



