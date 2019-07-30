module Base.Web(
    module Base.Web.Config
  , module Base.Web.Serve
  , module Base.Web.Swagger
  , module Base.Web.Middleware
  , module Base.Web.Actuator

  , module Base.Middleware.Trace
  , module Base.Actuator.Health
  , module Base.Actuator.Refresh

  , pluginWeb
  , HasSwagger
  ) where

import           Base.Web.Actuator
import           Base.Web.Config
import           Base.Web.Middleware
import           Base.Web.Serve
import           Base.Web.Swagger

import           Base.Actuator.Health
import           Base.Actuator.Refresh
import           Base.Middleware.Trace

import           Boots
import           Control.Exception
    ( Exception (..)
    , SomeException
    )
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Data.Function                       ((&))
import           Data.Maybe
import           Data.String
import           Data.Version                        (Version)
import           Lens.Micro.Extras
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Server.Internal.ServerError (responseServerError)
import           Servant.Swagger



whenException :: SomeException -> Network.Wai.Response
whenException e = responseServerError $ fromMaybe err400 { errBody = fromString $ show e} (fromException e :: Maybe ServerError)

serveWarp :: WebConfig -> Application -> IO ()
serveWarp WebConfig{..} = runSettings
  $ defaultSettings
  & setPort (fromIntegral port)
  & setOnException (\_ _ -> return ())
  & setOnExceptionResponse whenException

pluginWeb
  :: forall env m n api
  . ( MonadIO n
    , MonadThrow n
    , HasWebConfig env
    , HasLogger env
    , HasSalak env
    , HasServeWeb '[env] env
    , HasHealth env
    , HasSwaggerProxy env
    , HasMiddleware env
    , HasNatureT env m env
    , HasSwagger api
    , HasServer api '[env])
  => Version
  -> Proxy m
  -> Proxy api
  -> ServerT api m
  -> Plugin env n (IO ())
pluginWeb v pn proxy server = do
  config <- view askWebConfig <$> ask
  logInfo $ "Start Service [" <> name config <> "] ..."
  let proxycxt     = Proxy @'[env]
  ac  <- require "actuator"
  env <- combine
    [ pluginTrace @env pn
    , actuator proxycxt (Proxy @(Health :<|> Refresh)) ac
    , swaggerPlugin v proxycxt proxy
    ]
  logInfo $ "Service started on port(s): " <> fromString (show $ port config)
  let ServeWeb f = view askServeWeb env
      NatureT nt = view askNT env
  return
    $ serveWarp config
    $ view askMiddleware env
    $ f (Proxy @(Vault :> api)) (env :. EmptyContext)
    $ \val -> hoistServerWithContext proxy proxycxt (nt pn val env) server
