module Base.Middleware.Actuator where

import           Base.Actuator.Health
import           Base.Actuator.Info
import           Base.Actuator.Metrics
import           Base.Actuator.Refresh
import           Base.Dto
import           Base.Health
import           Base.Web.Types
import           Boots
import           Control.Monad.Catch
import           Control.Monad.Reader
import           Data.Proxy

actuators
  ::( HasSalak env
    , HasLogger env
    , HasApp env
    , HasHealth env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy (m :: * -> *) -> Proxy cxt -> Plugin env n env
actuators pm pc = do
  ac <- require "actuator"
  combine
    [ actuatorHealth  pm pc ac
    , actuatorInfo    pm pc ac
    , actuatorRefresh pm pc ac
    , actuatorMetrics pm pc ac
    ]








