module Base.Dto where
import           Data.String
import           Data.Text         (Text)
import           Data.Version      (Version)
import           Data.Word
import           Lens.Micro
import           Numeric
import           System.Random.MWC

data AppContext = AppContext
  { name :: !Text
  , ver  :: !Version
  , inst :: !Text
  , genr :: !GenIO
  }


class HasApp env where
  askApp :: Lens' env AppContext

instance HasApp AppContext where
  askApp = id

random64 :: IsString s => GenIO -> IO s
random64 z = do
  i <- uniform z :: IO Word64
  return $ fromString $ let x = showHex i "" in replicate (16 - length x) '0' ++ x
