module Base.Dto where
import           Data.Text    (Text)
import           Data.Version (Version)
import           Lens.Micro

data AppContext = AppContext
  { name :: !Text
  , ver  :: !Version
  , inst :: !Text
  } deriving Show


class HasApp env where
  askApp :: Lens' env AppContext

instance HasApp AppContext where
  askApp = id
