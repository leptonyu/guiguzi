module Base.Middleware.Error where

import           Base.Web.Types
import           Boots
import           Control.Exception (SomeException, catch, throw)
import           Data.Text         (pack)

buildError
  :: (HasWeb m cxt env, MonadIO n, HasLogger env)
  => Factory n env env
buildError = do
  lf <- askLoggerIO
  buildMiddleware
    $ \app req resH -> app req
      $ \res -> resH res `catch`
          \e -> runLoggingT (logError $ pack $ show (e :: SomeException)) lf >> throw e
