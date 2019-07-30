module Base.Actuator.Health where

import           Base.Web.Actuator
import           Base.Web.Serve
import           Base.Web.Swagger
import           Boots
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Swagger.Schema (ToSchema)
import           Data.Text           (Text)
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.Extras
import           Servant

data HealthStatus = UP | DOWN deriving (Eq, Show, Generic, ToJSON)

instance ToSchema HealthStatus
instance ToSchema Health

data Health = Health
  { status  :: HealthStatus
  , errMsg  :: Maybe Text
  , details :: HM.HashMap Text Health
  } deriving (Eq, Show, Generic)

emptyHealth :: IO Health
emptyHealth = return (Health UP Nothing HM.empty)

instance ToJSON Health where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

type HealthEndpoint = "health" :> Get '[JSON] Health

class HasHealth env where
  askHealth :: Lens' env (IO Health)

instance
  ( HasServeWeb cxt env
  , HasSalak env
  , HasHealth env
  , HasLogger env
  , HasSwaggerProxy env) => Actuator cxt env Health where
  actuator p _ ac = do
    env     <- ask
    mkActuator "health" ac p (Proxy @HealthEndpoint) (liftIO (view askHealth env))
