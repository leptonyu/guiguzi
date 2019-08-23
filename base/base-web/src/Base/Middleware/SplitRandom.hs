module Base.Middleware.SplitRandom where

import           Base.Web.Types
import           Boots
import           Control.Concurrent.MVar
import           Data.Maybe
import qualified Data.Vault.Lazy         as L
import           Lens.Micro
import           Lens.Micro.Extras
import           Network.Wai
import           System.Random.SplitMix


{-# INLINE buildSplitRandom #-}
buildSplitRandom
  ::( HasLogger env
    , HasApp cxt env
    , HasApp cxt cxt
    , HasVault cxt env
    , HasWeb m cxt env
    , MonadIO n)
  => Factory n env env
buildSplitRandom = do
  AppEnv{..} <- asks (view askApp)
  ranKey <- modifyContext $ over askApp . go
  buildMiddleware $ \app req resH -> do
    seed <- initSMGen >>= newMVar
    app req { vault = L.insert ranKey seed $ vault req } resH
  where
    {-# INLINE go #-}
    go v AppEnv{..} = AppEnv{randSeed = fromMaybe randSeed v, ..}