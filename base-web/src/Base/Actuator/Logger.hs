module Base.Actuator.Logger where

import           Base.Actuator
import           Base.Web.Types
import           Boots
import           Data.Aeson
import           Data.Swagger.Schema (ToSchema)
import           Data.Text           (Text, toLower)
import           GHC.Generics
import           Lens.Micro.Extras
import           Salak
import           Servant

type LoggerEndpoint = "logger" :> (Get '[JSON] LogInfo :<|> ReqBody '[JSON] LogInfo :> Put '[JSON] NoContent)

data LogInfo = LogInfo
  { level :: Text
  } deriving (Show, Generic, ToJSON, FromJSON, ToSchema)


toStr :: LogLevel -> Text
toStr LevelDebug     = "DEBUG"
toStr LevelInfo      = "INFO"
toStr LevelWarn      = "WARN"
toStr LevelError     = "ERROR"
toStr (LevelOther l) = l

fromStr :: Text -> LogLevel
fromStr "debug" = LevelDebug
fromStr "info"  = LevelInfo
fromStr "warn"  = LevelWarn
fromStr "error" = LevelError
fromStr other   = LevelOther other

actuatorLogger
  ::( HasSalak env
    , HasLogger env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => Proxy m -> Proxy cxt -> ActuatorConfig -> Factory n env env
actuatorLogger pm pc ac = do
  wll <- asks (view askLogLevel)
  newActuator pm pc ac "logger" (Proxy @LoggerEndpoint) (getLogInfo wll :<|> putLogInfo wll)
  where
    getLogInfo w = liftIO $ LogInfo . toStr <$> getWritable w
    putLogInfo w LogInfo{..} = liftIO $ setWritable (Just $ fromStr $ toLower level) w >> return NoContent
