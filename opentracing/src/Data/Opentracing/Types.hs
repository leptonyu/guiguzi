module Data.Opentracing.Types where

import           Data.ByteString       (ByteString)
import qualified Data.HashMap.Lazy     as HM
import           Data.Scientific
import           Data.String
import           Data.Text             (Text)
import           Data.Time.Clock.POSIX
import           Data.Word
import           Lens.Micro
import           Numeric
import           System.Random.MWC

data SpanTag
  = TagString Text
  | TagBool   Bool
  | TagNum    Scientific
  deriving (Eq, Show)

data SpanContext = SpanContext
  { traceId :: ByteString
  , baggage :: HM.HashMap Text (Maybe Text)
  , gen     :: IO ByteString
  , getNow  :: IO Int
  , notify  :: Span -> IO ()
  }

data SpanReferenceType
  = FollowsFrom
  | ChildOf
  deriving (Eq, Show)

data SpanReference = SpanReference
  { referenceType :: SpanReferenceType
  , parentId      :: ByteString
  } deriving (Eq, Show)

data Span = Span
  { spanId     :: ByteString
  , name       :: Text
  , startTime  :: Int
  , finishTime :: Maybe Int
  , tags       :: HM.HashMap Text SpanTag
  , logs       :: HM.HashMap Text Text
  , references :: [SpanReference]
  } deriving (Eq, Show)

data Trace = Trace
  { context :: SpanContext
  , spans   :: SpanRef
  }

data SpanRef = NoSpan | SpanId ByteString | SpanRef Span

newSpanContext :: IO SpanContext
newSpanContext = do
  y       <- createSystemRandom
  let gen = go y
  traceId <- gen
  let baggage = HM.empty
      getNow  = round <$> getPOSIXTime
      notify _= return ()
  return SpanContext{..}
  where
    go z = do
      i <- uniform z :: IO Word64
      return $ fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x

class HasTrace env where
  askTrace :: Lens' env Trace

instance HasTrace Trace where
  askTrace = id





