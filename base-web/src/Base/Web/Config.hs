module Base.Web.Config where

import           Boots
import           Data.Default
import           Data.Text    (Text)
import           Data.Word
import           Lens.Micro
import           Salak
import           Servant

-- | Application Configuration.
data WebConfig = WebConfig
  { name     :: Text   -- ^ Application name.
  , hostname :: String -- ^ Applicatoin hostname, used in swagger.
  , port     :: Word16 -- ^ Application http port.
  } deriving (Eq, Show)

instance Monad m => FromProp m WebConfig where
  fromProp = WebConfig
    <$> "name" .?= "application"
    <*> "host" .?= "localhost"
    <*> "port" .?= 8888


class HasWebConfig env where
  askWebConfig :: Lens' env WebConfig

instance HasWebConfig WebConfig where
  askWebConfig = id


data NatureT cxt = NatureT { unNT :: forall a. Vault -> cxt -> App cxt a -> Handler a }

class HasNatureT cxt env where
  askNT :: Lens' env (NatureT cxt)

instance HasNatureT cxt (NatureT cxt) where
  askNT = id

instance Default (NatureT cxt) where
  def = NatureT $ \_ e -> liftIO . runAppT e
