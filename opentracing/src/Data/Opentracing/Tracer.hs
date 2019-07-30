module Data.Opentracing.Tracer(
    SpanName
  , finishSpan
  , setBaggage
  , forkSpan
  , newChildSpan
  ) where

import           Control.Exception      (SomeException, catch)
import qualified Data.HashMap.Lazy      as HM
import           Data.Opentracing.Types
import           Data.Text              (Text)

type SpanName = Text

finishSpan :: Trace -> IO ()
finishSpan Trace{..} = case spans of
  SpanRef s -> do
    endT <- getNow context
    notify context s {finishTime = Just endT }
  _         -> return ()

setBaggage :: Text -> Maybe Text -> SpanContext -> SpanContext
setBaggage k t SpanContext{..} = SpanContext{ baggage = HM.insert k t baggage , .. }

newSpan :: SpanReferenceType -> SpanName -> Trace -> IO Trace
newSpan t name Trace{..} = do
  let rf = case spans of
            SpanRef sp -> [SpanReference t $ spanId sp]
            SpanId  si -> [SpanReference t si]
            _          -> []
  s <- SpanRef <$> go rf context
  return Trace { spans = s, .. }
  where
    go references SpanContext{..} = do
      let finishTime = Nothing
          tags       = HM.empty
          logs       = HM.empty
      spanId    <- if null references then return traceId else gen
      startTime <- getNow
      let s = Span{..}
      notify s `catch` (\(_ :: SomeException) -> return ())
      return s

forkSpan :: SpanName -> Trace -> IO Trace
forkSpan = newSpan FollowsFrom

newChildSpan :: SpanName -> Trace -> IO Trace
newChildSpan = newSpan ChildOf

