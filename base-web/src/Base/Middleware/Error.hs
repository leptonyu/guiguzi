module Base.Middleware.Error where

import           Base.Web.Types
import           Boots
import           Control.Exception              (SomeException, catch, throw)
import           Control.Monad.IO.Class
import           Control.Monad.Logger.CallStack
import           Data.Kind                      (Type)
import           Data.Proxy
import           Data.Text                      (pack)

pluginError
  :: (HasWeb m cxt env, MonadIO n, HasLogger env)
  => Proxy (m :: Type -> Type) -> Proxy cxt -> Plugin env n env
pluginError pm pc = do
  lf <- askLoggerIO
  middlewarePlugin pm pc
    $ \app req resH -> app req
      $ \res -> resH res `catch`
          \e -> runLoggingT (logError $ pack $ show (e :: SomeException)) lf >> throw e
