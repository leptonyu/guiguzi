module Base.Actuator.Info where

import           Base.Actuator
import           Base.Web.Types
import           Boots
import           Control.Concurrent
import           Data.Aeson
import           Data.Swagger.Schema (ToSchema)
import           Data.Text           (Text)
import           Data.Version
import           GHC.Generics
import           GHC.RTS.Flags
import           Lens.Micro.Extras
import           Servant
import           System.Info

data Info = Info
  { name    :: !Text
  , version :: !Version
  , profile :: !Bool
  } deriving (Show, Generic, ToSchema)

type InfoEndpoint = "info" :> Get '[JSON] Info

actuatorInfo
  :: forall m cxt env n
  . ( HasSalak env
    , HasLogger env
    , HasApp cxt env
    , HasWeb m cxt env
    , MonadIO m
    , MonadIO n
    , MonadThrow n)
  => ActuatorConfig -> Factory n env env
actuatorInfo ac = do
  app <- asks (view askApp)
  newActuator ac "info" (Proxy @InfoEndpoint) $ liftIO $ do
    rtsf <- getRTSFlags
    return (go rtsf app)
  where
    {-# INLINE go #-}
    go RTSFlags{..} (AppEnv{..} :: AppEnv cxt) =
      let ProfFlags{..} = profilingFlags
      in Info{profile = find doHeapProfile , ..}
    {-# INLINE find #-}
    find NoHeapProfiling = False
    find _               = True

instance ToJSON Info where
  toJSON Info{..} = object
    [ "application"   .= name
    , "version"       .= version
    , "isMultithread" .= rtsSupportsBoundThreads
    , "isProfile"     .= profile
    , "os"            .= os
    , "arch"          .= arch
    , "compiler"      .= (compilerName <> "-" <> showVersion compilerVersion)
    ]
