module Base.Actuator.Refresh where

import           Base.Actuator
import           Base.Web.Types
import           Boots
import           Data.Aeson
import           Data.Swagger.Schema (ToSchema)
import           GHC.Generics
import           Salak
import           Servant

type RefreshEndpoint = "refresh" :> Post '[JSON] Refresh

data Refresh = Refresh
  { hasError :: !Bool
  , msgs     :: ![String]
  } deriving (Eq, Show, Generic, ToJSON, ToSchema)

actuatorRefresh
  ::( HasSalak env
    , HasLogger env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy m -> Proxy cxt -> ActuatorConfig -> Factory n env env
actuatorRefresh pm pc ac = do
  reload <- askReload
  newActuator pm pc ac "refresh" (Proxy @RefreshEndpoint) (liftIO $ go <$> reload)
  where
    go ReloadResult{..} = Refresh{..}
