module Network.Consul where

import qualified Data.HashMap.Strict as HM
import           Data.Word
import           Servant.API

data ServiceKind
  = KindNil
  | KindConnectProxy
  | KindMeshGateway
  deriving (Eq, Show)

data ServiceConnect = ServiceConnect
  { enabled :: Bool
  } deriving (Eq, Show)

data ServiceCheck = ServiceCheck
  {

  } deriving (Eq, Show)

data ServiceWeight = ServiceWeight
  {

  } deriving (Eq, Show)


data ServiceDef = ServiceDef
  { sname        :: !String
  , sid          :: !(Maybe String)
  , stags        :: ![String]
  , saddr        :: !(Maybe String)
  , saddrmap     :: !(HM.HashMap String String)
  , smeta        :: !(HM.HashMap String String)
  , sport        :: !Word16
  , skind        :: !ServiceKind
  , sconnect     :: !(Maybe ServiceConnect)
  , scheck       :: !(Maybe ServiceCheck)
  , schecks      :: ![ServiceCheck]
  , stagoverride :: !Bool
  , sweights     :: !(Maybe ServiceWeight)
  } deriving (Eq, Show)



-- class MonadConsul m where
--   registerService ::



type ConsulEndpoint
  = "agent" :> "services" :> Get '[JSON] ServiceDef















