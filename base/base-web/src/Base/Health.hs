module Base.Health where

import           Boots
import           Control.Concurrent.MVar
import           Control.Exception       (SomeException, catch)
import           Data.Aeson
import qualified Data.HashMap.Strict     as HM
import           Data.Swagger.Schema     (ToSchema)
import           Data.Text               (Text, pack)
import           GHC.Generics
import           Lens.Micro
import           Lens.Micro.Extras

data HealthStatus = UP | DOWN deriving (Eq, Show, Generic, ToJSON)

instance ToSchema HealthStatus
instance ToSchema Health

data Health = Health
  { status  :: !HealthStatus
  , errMsg  :: !(Maybe Text)
  , details :: !(HM.HashMap Text Health)
  } deriving (Eq, Show, Generic)

newtype HealthRef = HealthRef (MVar (IO Health))

{-# INLINE emptyHealth #-}
emptyHealth :: IO HealthRef
emptyHealth = do
  let io = return (Health UP Nothing HM.empty)
  f <- newMVar io
  return (HealthRef f)

instance ToJSON Health where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

class HasHealth env where
  askHealth :: Lens' env HealthRef

instance HasHealth HealthRef where
  askHealth = id

type CheckHealth = (Text, IO HealthStatus)

insertHealth :: CheckHealth -> IO Health -> IO Health
insertHealth (na, ios) ior = do
  (err,s)          <- ((Nothing,) <$> ios) `catch` (\(e :: SomeException) -> return (Just (pack $ show e), DOWN))
  Health{..} <- ior
  return (Health (if s == DOWN then s else status) Nothing $ HM.insert na (Health s err HM.empty) details)

combineHealth :: [CheckHealth] -> IO Health -> IO Health
combineHealth = flip (foldr insertHealth)

buildHealth
  :: forall env n. (MonadIO n, HasHealth env)
  => CheckHealth
  -> Factory n env ()
buildHealth healths = do
  HealthRef href <- asks (view askHealth)
  liftIO $ modifyMVar_ href $ return . insertHealth healths





