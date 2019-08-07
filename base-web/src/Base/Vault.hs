module Base.Vault where

import           Boots
import           Control.Concurrent.MVar
import           Lens.Micro
import           Lens.Micro.Extras
import           Servant

data VaultCake env  = VaultCake (Vault -> env -> env)

type VaultCakeRef env = MVar (VaultCake env)

class HasVault cxt env where
  askVault :: Lens' env (VaultCakeRef cxt)

instance HasVault cxt (VaultCakeRef cxt) where
  askVault = id

newVaultCake :: IO (VaultCakeRef cxt)
newVaultCake = newMVar (VaultCake $ \_ -> id)

modifyVault :: (HasVault cxt env, MonadIO n) => ((Vault -> cxt -> cxt) -> Vault -> cxt -> cxt) -> Factory n env ()
modifyVault f = do
  ref <- asks (view askVault)
  liftIO $ modifyMVar_ ref $ \(VaultCake g) -> return $ VaultCake (f g)

runVault :: (MonadIO m, HasVault env env) => env -> Vault -> AppT env m a -> m a
runVault env v ma = do
  VaultCake f <- liftIO $ readMVar $ view askVault env
  runAppT (f v env) ma



