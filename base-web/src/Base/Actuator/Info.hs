module Base.Actuator.Info where

import           Base.Actuator
import           Base.Dto
import           Base.Web.Types
import           Boots
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Swagger.Schema  (ToSchema)
import           Data.Text            (Text)
import           Data.Version         (Version)
import           GHC.Generics
import           Lens.Micro.Extras
import           Servant

data Info = Info
  { name :: Text
  , ver  :: Version
  } deriving (Show, Generic, ToSchema)

type InfoEndpoint = "info" :> Get '[JSON] Info

actuatorInfo
  ::( HasSalak env
    , HasLogger env
    , HasApp env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy m -> Proxy cxt -> ActuatorConfig -> Plugin env n env
actuatorInfo pm pc ac = asks (view askApp)
  >>= newActuator pm pc ac "info" (Proxy @InfoEndpoint) . return . go
  where
    go AppContext{..} = Info{..}

instance ToJSON Info where
  toJSON Info{..} = object [ "application" .= name, "version" .= ver ]
