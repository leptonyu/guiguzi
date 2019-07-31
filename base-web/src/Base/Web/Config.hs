module Base.Web.Config where

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

instance Default WebConfig where
  def = WebConfig "application" "localhost" 8888


instance Monad m => FromProp m WebConfig where
  fromProp = WebConfig
    <$> "name" .?: name
    <*> "host" .?: hostname
    <*> "port" .?: port


class HasWebConfig env where
  askWebConfig :: Lens' env WebConfig

instance HasWebConfig WebConfig where
  askWebConfig = id

data NatureT cxt m = NatureT { unNT :: forall a. Proxy m -> Vault -> cxt -> m a -> Handler a }

class HasNatureT cxt m env where
  askNT :: Lens' env (NatureT cxt m)

instance HasNatureT cxt m (NatureT cxt m) where
  askNT = id

modifyNT :: forall m cxt. ((Vault,cxt) -> (Vault, cxt)) -> NatureT cxt m -> NatureT cxt m
modifyNT f (NatureT nt) = NatureT $ \p v c -> let (v1,c1) = f (v,c) in nt p v1 c1

modifyNT' :: forall m cxt env. HasNatureT cxt m env => ((Vault,cxt) -> (Vault, cxt)) -> env -> env
modifyNT' = over askNT . modifyNT @m








