module Base.Web.Types where

import           Base.Web.Config
import           Data.Default
import           Data.Proxy
import           Data.Swagger
import           Lens.Micro
import           Network.Wai
import           Servant
import           Servant.Swagger


data Web m cxt = Web
  { config :: WebConfig
  , nature :: forall a. Proxy m -> Vault -> cxt -> m a -> Handler a
  , middle :: Middleware
  , serveW :: forall api. HasServer api '[cxt] => Proxy api -> Context '[cxt] -> Server api -> Application
  , swagge :: forall api. HasSwagger api => Proxy api -> Swagger
  }

class HasWeb cxt m env where
  askWeb :: Lens' env (Web m cxt)

instance HasWeb cxt m (Web m cxt) where
  askWeb = id

instance Default (Web Handler cxt) where
  def = Web def (\_ _ _ -> id) id serveWithContext toSwagger
