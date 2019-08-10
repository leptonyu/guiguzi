module Base.Metrics(
    Store
  , newStore
  , HasMetrics(..)
  ) where

import           Lens.Micro
import           System.Metrics


class HasMetrics env where
  askMetrics :: Lens' env Store
