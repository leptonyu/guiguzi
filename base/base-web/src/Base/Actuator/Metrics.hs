module Base.Actuator.Metrics where

import           Base.Actuator
import           Base.Metrics
import           Base.Web.Types
import           Boots
import           Control.Exception      (SomeException, catch, throw)
import qualified Data.HashMap.Strict    as HM
import           Data.Proxy
import           Data.Text              (Text, pack)
import           Lens.Micro.Extras
import           Servant
import           System.Metrics
import qualified System.Metrics.Counter as Counter

type MetricsEndpoint = "metrics" :> Get '[JSON] Metrics

type Metrics = HM.HashMap Text Text

actuatorMetrics
  ::( HasSalak env
    , HasLogger env
    , HasMetrics env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => ActuatorConfig -> Factory n env env
actuatorMetrics ac = do
  store <- asks (view askMetrics)
  liftIO $ registerGcMetrics store
  let newC n = liftIO $ createCounter n store
  requests <- newC "http.server.requests"
  req_fail <- newC "http.server.requests.failure"
  mconcat
    [ buildMiddleware
      $ \app req resH -> app req
      $ \res -> Counter.inc requests
        >> resH res `catch` (\(e :: SomeException) -> Counter.inc req_fail >> throw e)
    , newActuator ac "metrics" (Proxy @MetricsEndpoint) (liftIO $ go store)
    ]
  where
    go s = do
      sample <- sampleAll s
      return (HM.map g2 sample)
    showT :: Show a => a -> Text
    showT = pack . show
    g2 (Counter i)      = showT i
    g2 (Gauge i)        = showT i
    g2 (Label x)        = x
    g2 (Distribution i) = showT i


