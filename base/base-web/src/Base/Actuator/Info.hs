module Base.Actuator.Info where

import           Base.Actuator
import           Base.Web.Types
import           Boots
import           Data.Aeson
import           Data.Swagger.Schema (ToSchema)
import           Data.Text           (Text)
import           Data.Version        (Version)
import           GHC.Generics
import           Lens.Micro.Extras
import           Servant

data Info = Info
  { name    :: !Text
  , version :: !Version
  } deriving (Show, Generic, ToSchema)

type InfoEndpoint = "info" :> Get '[JSON] Info

actuatorInfo
  :: forall m cxt env n
  . ( HasSalak env
    , HasLogger env
    , HasApp cxt env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => ActuatorConfig -> Factory n env env
actuatorInfo ac = asks (view askApp)
  >>= newActuator ac "info" (Proxy @InfoEndpoint) . return . go
  where
    go (AppEnv{..} :: AppEnv cxt) = Info{..}

instance ToJSON Info where
  toJSON Info{..} = object [ "application" .= name, "version" .= version ]
