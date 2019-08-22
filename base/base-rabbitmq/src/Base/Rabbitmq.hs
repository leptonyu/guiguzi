module Base.Rabbitmq where

import           Boots
import           Network.AMQP
import           Network.Socket (PortNumber)
import           Salak

instance Monad m => FromProp m PortNumber where
  fromProp = fromInteger <$> fromProp

instance FromProp m ConnectionOpts where
  fromProp = ConnectionOpts
    <$> fmap (:[]) go
    <*> "vhost" .?= "/"
    <*> fmap (:[]) g2
    <*> "max-frame-size"  .?= Just 131072
    <*> "heartbeat-delay"
    <*> "max-chanel"
    <*> return Nothing
    <*> "name"
    where
      go :: Prop m (String, PortNumber)
      go = (,)
          <$> "hostname" .?= "localhost"
          <*> "port"     .?= 5672
      g2 :: Prop m SASLMechanism
      g2 = plain
          <$> "username" .?= "guest"
          <*> "password" .?= "guest"

newtype Rabbitmq = Rabbitmq Connection
newtype RabbitmqChannel = RabbitmqChannel Channel

buildRabbitmq :: (MonadMask n, MonadIO n, HasSalak env) => Factory n env Rabbitmq
buildRabbitmq = do
  opts <- require "rabbitmq"
  conn <- bracket (liftIO $ openConnection'' opts) (liftIO . closeConnection)
  return (Rabbitmq conn)

buildRabbitmqChannel :: (MonadMask n, MonadIO n) => Rabbitmq -> Factory n env RabbitmqChannel
buildRabbitmqChannel (Rabbitmq conn) = do
  ch <- bracket (liftIO $ openChannel conn) (liftIO . closeChannel)
  return (RabbitmqChannel ch)
