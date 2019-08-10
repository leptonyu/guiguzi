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

buildActuators
  ::( HasSalak env
    , HasLogger env
    , HasApp cxt env
    , HasMetrics env
    , HasHealth env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Factory n env env
buildActuators = do
  ac <- require "actuator"
  mconcat
    [ actuatorHealth  ac
    , actuatorInfo    ac
    , actuatorRefresh ac
    , actuatorMetrics ac
    , actuatorLogger  ac
    ]








