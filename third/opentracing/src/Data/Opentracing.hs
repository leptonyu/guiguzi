module Data.Opentracing(
    runWithTrace
  , module Data.Opentracing.Tracer
  , module Data.Opentracing.Types
  ) where

import           Control.Exception       (finally)
import           Data.Opentracing.Tracer
import           Data.Opentracing.Types

runWithTrace :: SpanName -> Trace -> (Trace -> IO a) -> IO a
runWithTrace name trace f = do
  t <- newChildSpan name trace
  f t `finally` finishSpan t
