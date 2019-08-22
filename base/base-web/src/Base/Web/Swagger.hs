{-# LANGUAGE TypeFamilies #-}
module Base.Web.Swagger where

import qualified Data.Swagger    as S
import           Data.Text       (Text, pack)
import           Data.Version
import           GHC.TypeLits
import           Lens.Micro
import           Salak
import           Servant
import           Servant.Swagger

-- ** Swagger
-- | Swagger Configuration
data SwaggerConfig = SwaggerConfig
  { urlDir    :: String -- ^ Url path for swagger.
  , urlSchema :: String -- ^ Api schema path for swagger.
  , enabled   :: Bool   -- ^ If enable swagger.
  } deriving (Eq, Show)

instance FromProp m SwaggerConfig where
  fromProp = SwaggerConfig
    <$> "dir"     .?= "swagger-ui"
    <*> "schema"  .?= "swagger-ui.json"
    <*> "enabled" .?= True


data SwaggerTag (name :: Symbol) (desp :: Symbol)

-- | Swagger modification
baseInfo
  :: String  -- ^ Hostname
  -> Text    -- ^ Server Name
  -> Version -- ^ Server version
  -> Int     -- ^ Port
  -> S.Swagger -- ^ Old swagger
  -> S.Swagger
baseInfo hostName n v p s = s
  & S.info . S.title   .~ (n <> " API Documents")
  & S.info . S.version .~ pack (showVersion v)
  & S.host ?~ S.Host hostName (Just $ fromIntegral p)

instance HasServer api ctx
  => HasServer (SwaggerTag name desp :> api) ctx where
  type ServerT (SwaggerTag name desp :> api) m = ServerT api m
  route _ = route (Proxy @api)
  hoistServerWithContext _ = hoistServerWithContext (Proxy @api)

instance (HasSwagger api, KnownSymbol name, KnownSymbol desp)
  => HasSwagger (SwaggerTag name desp :> api) where
  toSwagger _ = toSwagger (Proxy @api) & S.applyTags [tag]
    where
      {-# INLINE tag #-}
      tag = S.Tag (go (Proxy @name)) (g2 $ go (Proxy @desp)) Nothing
      {-# INLINE go #-}
      go :: forall a. KnownSymbol a => Proxy a -> Text
      go  = pack . symbolVal
      {-# INLINE g2 #-}
      g2 "" = Nothing
      g2 a  = Just a
