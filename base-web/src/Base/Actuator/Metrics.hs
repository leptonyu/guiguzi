module Base.Actuator.Metrics where

import           Base.Actuator
import           Base.Dto
import           Base.Web.Types
import           Boots
import           Control.Exception      (SomeException, catch, throw)
import           Control.Monad.Reader
import qualified Data.HashMap.Strict    as HM
import           Data.Proxy
import           Data.Text              (Text, pack)
import           Lens.Micro.Extras
import           Salak
import           Servant
import           System.Metrics
import qualified System.Metrics.Counter as Counter

type MetricsEndpoint = "metrics" :> Get '[JSON] Metrics

type Metrics = HM.HashMap Text Text

actuatorMetrics
  ::( HasSalak env
    , HasLogger env
    , HasApp env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy m -> Proxy cxt -> ActuatorConfig -> Plugin env n env
actuatorMetrics pm pc ac = do
  AppContext{..} <- asks (view askApp)
  store          <- liftIO newStore
  liftIO $ registerGcMetrics store
  let newC n = liftIO $ createCounter (name <> "." <> n) store
  requests <- newC "requests"
  req_fail <- newC "requests.failure"
  combine
    [ middlewarePlugin pm pc
      $ \app req resH -> app req
      $ \res -> Counter.inc requests
        >> resH res `catch` (\(e :: SomeException) -> Counter.inc req_fail >> throw e)
    , newActuator pm pc ac "metrics" (Proxy @MetricsEndpoint) (liftIO $ go store)
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


