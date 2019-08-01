module Base.Actuator.Refresh where

import           Base.Middleware.Actuator
import           Base.Web.Types
import           Boots
import           Data.Aeson
import           Data.Swagger.Schema      (ToSchema)
import           GHC.Generics
import           Salak
import           Servant

type RefreshEndpoint = "refresh" :> Post '[JSON] Refresh

data Refresh = Refresh
  { hasError :: Bool
  , msgs     :: [String]
  } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance
  ( HasWeb m cxt env
  , HasLogger env
  , HasSalak env
  , MonadIO m) => Actuator m cxt env Refresh where
  actuator pm pc _ ac = do
    reload  <- askReload
    mkActuator pm pc "refresh" ac (Proxy @RefreshEndpoint) (go <$> liftIO reload)
    where
      go ReloadResult{..} = Refresh{..}
