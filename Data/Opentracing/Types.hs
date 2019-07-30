module Data.Opentracing.Types where

import           Data.ByteString   (ByteString)
import qualified Data.HashMap.Lazy as HM
import           Data.Scientific
import           Data.Text         (Text)

data SpanTag
  = TagString Text
  | TagBool   Bool
  | TagNum    Scientific
  deriving (Eq, Show)

data SpanContext = SpanContext
  { traceId :: ByteString
  , baggage :: HM.HashMap Text (Maybe Text)
  } deriving (Eq, Show)

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
  , context    :: SpanContext
  , references :: [SpanReference]
  } deriving (Eq, Show)
