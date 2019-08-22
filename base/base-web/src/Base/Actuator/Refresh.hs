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
  => ActuatorConfig -> Factory n env env
actuatorRefresh ac = do
  reload <- askReload
  newActuator ac "refresh" (Proxy @RefreshEndpoint) (liftIO $ go <$> reload)
  where
    {-# INLINE go #-}
    go ReloadResult{..} = Refresh{..}
