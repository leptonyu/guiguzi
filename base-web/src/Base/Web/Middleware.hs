module Base.Web.Middleware where

import           Boots
import           Lens.Micro
import           Network.Wai


class HasMiddleware env where
  askMiddleware :: Lens' env Middleware

instance HasMiddleware Middleware where
  askMiddleware = id

middlewarePlugin :: HasMiddleware env => Middleware -> Plugin env m env
middlewarePlugin md = over askMiddleware (.md) <$> ask
