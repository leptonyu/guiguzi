module Base.Web.Serve where

import           Data.Default
import           Lens.Micro
import           Servant

data ServeWeb cxt = ServeWeb
  { unServeWeb :: forall api. HasServer api cxt
    => Proxy api
    -> Context cxt
    -> Server api
    -> Application
  }

instance Default (ServeWeb cxt) where
  def = ServeWeb serveWithContext

tryServeWeb
  :: forall api cxt m
  . HasServer api cxt
  => (Context cxt -> (forall a. m a -> Handler a))
  -> Bool
  -> Proxy api
  -> ServerT api m
  -> ServeWeb cxt
  -> ServeWeb cxt
tryServeWeb nt b p s (ServeWeb f) = ServeWeb
  $ if b
      then \p0 c s0 -> f (gop p p0) c (hoistServerWithContext p (Proxy @cxt) (nt c) s :<|> s0)
      else f

gop :: forall a b. Proxy a -> Proxy b -> Proxy (a :<|> b)
gop _ _ = Proxy

class HasServeWeb cxt env where
  askServeWeb :: Lens' env (ServeWeb cxt)

instance HasServeWeb cxt (ServeWeb cxt) where
  askServeWeb = id
