module Base.Web.Types where

import Base.Web.Config
import Servant
import Data.Proxy
import Network.Wai
import Servant.Swagger
import Data.Swagger


data Web cxt m = Web
  { config :: WebConfig
  , nature :: forall a. Proxy m -> Vault -> cxt -> m a -> Handler a
  , middle :: Middleware
  , serveW :: forall api. HasServer api cxt => Proxy api -> Context cxt -> Server api -> Application
  , swagge :: forall api. HasSwagger api => Proxy api -> Swagger
  }



