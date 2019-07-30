module Base.Actuator.Refresh where

import           Base.Web.Actuator
import           Base.Web.Serve
import           Base.Web.Swagger
import           Boots
import           Data.Aeson
import           Data.Swagger.Schema (ToSchema)
import           GHC.Generics
import           Salak
import           Servant

type RefreshEndpoint = "refresh" :> Post '[JSON] Refresh

data Refresh = Refresh
  { hasError :: Bool
  , msgs     :: [String]
  } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance
  ( HasServeWeb cxt env
  , HasSwaggerProxy env
  , HasLogger env
  , HasSalak env) => Actuator cxt env Refresh where
  actuator p _ ac = do
    reload  <- askReload
    mkActuator "refresh" ac p (Proxy @RefreshEndpoint) (go <$> liftIO reload)
    where
      go ReloadResult{..} = Refresh{..}
