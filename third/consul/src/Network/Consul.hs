{-# LANGUAGE NoGeneralizedNewtypeDeriving #-}
module Network.Consul where

import           Base.Client
import           Boots
import           Data.Aeson
import           Data.ByteString              (ByteString)
import qualified Data.HashMap.Strict          as HM
import           Data.Proxy
import           Data.Swagger.Schema          (ToSchema)
import           Data.Text                    (Text, toLower)
import           Data.Word
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.Extras
import           Network.HTTP.Client          (newManager)
import           Network.HTTP.Client.Internal hiding (Proxy)
import           Salak
import           Servant.API
import           Servant.Client

data ServiceKind
  = KindNil
  | KindConnectProxy
  | KindMeshGateway
  deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)

newtype ServiceConnect = ServiceConnect
  { enabled :: Bool
  } deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)

data ServiceCheck = ServiceCheck
  { cname     :: !Text
  , cid       :: !Text
  , cinternal :: !String
  , cdcsa     :: !String
  , chttp     :: !String
  } deriving (Eq, Show, Generic, ToSchema)

instance ToJSON ServiceCheck where
  toJSON ServiceCheck{..} = object
    [ "ID"    .= cid
    , "Name"  .= cname
    , "Interval" .= cinternal
    , "DeregisterCriticalServiceAfter" .= cdcsa
    , "HTTP"  .= chttp
    ]
instance FromJSON ServiceCheck where
  parseJSON = withObject "ServiceCheck" $ \v -> ServiceCheck
    <$> v .: "Name"
    <*> v .: "ID"
    <*> v .: "Interval"
    <*> v .: "DeregisterCriticalServiceAfter"
    <*> v .:? "HTTP" .!= ""

data ServiceWeight = ServiceWeight
  {

  } deriving (Eq, Show, Generic, ToSchema, FromJSON, ToJSON)

data ServiceDef = ServiceDef
  { sname        :: !Text
  , sid          :: !Text
  , stags        :: ![Text]
  , saddr        :: !(Maybe String)
  , saddrmap     :: !(HM.HashMap String String)
  , smeta        :: !(HM.HashMap Text Text)
  , sport        :: !(Maybe Word16)
  , skind        :: !ServiceKind
  , sconnect     :: !(Maybe ServiceConnect)
  , scheck       :: !(Maybe ServiceCheck)
  , schecks      :: ![ServiceCheck]
  , stagoverride :: !Bool
  , sweights     :: !(Maybe ServiceWeight)
  } deriving (Eq, Show, Generic, ToSchema)

data HttpServer = HttpServer
  { sname :: !Text
  , sid   :: !Text
  , saddr :: !(Maybe String)
  , sport :: !(Maybe Word16)
  , stags :: ![Text]
  , smeta :: !(HM.HashMap Text Text)
  , chk   :: !ServiceCheck
  }

newServer :: HttpServer -> ServiceDef
newServer HttpServer{..} = ServiceDef
  { saddrmap = HM.empty
  , skind = KindNil
  , sconnect = Nothing
  , scheck = Just chk
  , schecks = []
  , sweights = Nothing
  , stagoverride = False
  ,..}

instance ToJSON ServiceDef where
  toJSON ServiceDef{..} = object
    [ "ID"      .= sid
    , "Name"    .= sname
    , "Address" .= saddr
    , "Port"    .= sport
    , "Tags"    .= toJSON stags
    , "Meta"    .= toJSON smeta
    , "Check"   .= toJSON scheck
    ]

instance FromJSON ServiceDef where
  parseJSON = withObject "ServiceDef" $ \v -> ServiceDef
    <$> v .: "Name"
    <*> v .:? "ID"   .!= ""
    <*> v .:? "Tags" .!= []
    <*> v .: "Address"
    <*> return HM.empty
    <*> v .:? "Meta" .!= HM.empty
    <*> v .: "Port"
    <*> return KindNil
    <*> return Nothing
    <*> return Nothing
    <*> return []
    <*> v .:? "EnableTagOverride" .!= True
    <*> return Nothing

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

consulApi
  :: (HasConsul env, MonadThrow m, MonadIO m)
  => Proxy m -> Proxy env -> HttpClient -> ConsulApi m env
consulApi pm p hc =
  let getServices
        :<|> getService
        :<|> registerService
        :<|> deregisterService
        :<|> maintenanceService
        :<|> checkHealthByName
        :<|> checkHealthById = hoistClient api (runConsul pm p hc) (client api)
  in ConsulApi{..}

api :: Proxy ConsulEndpoint
api = Proxy
runConsul
  :: (HasConsul env, MonadThrow m, MonadIO m)
  => Proxy m -> Proxy env -> HttpClient -> ClientM a -> AppT env m a
runConsul _ _ (HttpClient mg) cma = do
  env <- ask
  let ConsulConfig{..} = view askConsulConfig env
      mgn              = case token of
        Just t -> mg { managerModifyRequest = \req -> return req { requestHeaders = ("X-Consul-Token", t) : requestHeaders req }}
        _      -> mg
  liftIO $ do
    m <- newManager mgn
    v <- runClientM cma (ClientEnv m url Nothing)
    case v of
      Left  e -> throwM e
      Right a -> return a

consulServer pm p hc = hoistClient api (runConsul pm p hc) (client api)

data ConsulConfig = ConsulConfig
  { meta     :: HM.HashMap Text Text
  , tags     :: [Text]
  , token    :: Maybe ByteString
  , interval :: String
  , dcsa     :: String
  , url      :: BaseUrl
  }

instance FromProp m ConsulConfig where
  fromProp = ConsulConfig
    <$> "meta"
    <*> "tags"
    <*> "token"
    <*> "interval" .?= "10s"
    <*> "deregister-critical-service-after" .?= "30m"
    <*> (BaseUrl
      <$> "schema" .?= Http
      <*> "host"   .?= "127.0.0.1"
      <*> "port"   .?= 8500
      <*> "path"   .?= "")

instance FromProp m Scheme where
  fromProp = readEnum (go.toLower)
    where
      {-# INLINE go #-}
      go "http"  = Right Http
      go "https" = Right Https
      go _       = Left "unkown schema"

class HasConsul env where
  askConsulConfig :: Lens' env ConsulConfig

  askConsul :: forall m. (MonadThrow m, MonadIO m) => HttpClient -> ConsulApi m env
  askConsul = consulApi (Proxy @m) (Proxy @env)

instance HasConsul ConsulConfig where
  askConsulConfig = id


