module Base.Middleware.Actuator where

import           Base.Actuator.Health
import           Base.Actuator.Info
import           Base.Actuator.Logger
import           Base.Actuator.Metrics
import           Base.Actuator.Refresh
import           Base.Health
import           Base.Metrics
import           Base.Web.Types
import           Boots
import           Data.Kind             (Type)
import           Data.Proxy

buildActuators
  ::( HasSalak env
    , HasLogger env
    , HasApp env
    , HasMetrics env
    , HasHealth env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy (m :: Type -> Type) -> Proxy cxt -> Factory n env env
buildActuators pm pc = do
  ac <- require "actuator"
  mconcat
    [ actuatorHealth  pm pc ac
    , actuatorInfo    pm pc ac
    , actuatorRefresh pm pc ac
    , actuatorMetrics pm pc ac
    , actuatorLogger  pm pc ac
    ]








