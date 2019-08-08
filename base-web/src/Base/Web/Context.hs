module Base.Web.Context(
    askCxt
  , SetContextEntry(..)
  , HasContextEntry(..)
  ) where
import           Lens.Micro
import           Servant

askCxt ::  (SetContextEntry a b, HasContextEntry a b) => Lens' (Context a) b
askCxt = lens getContextEntry setContextEntry

class SetContextEntry a b where
  setContextEntry :: Context a -> b -> Context a

instance SetContextEntry (a : b) a where
  setContextEntry (_ :. b) a = (a :. b)

instance SetContextEntry b a => SetContextEntry (c : b) a where
  setContextEntry (c :. b) a = c :. setContextEntry b a
