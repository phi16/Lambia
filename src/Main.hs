import Prelude hiding (getContents)
import Control.Applicative
import Data.ByteString (pack, unpack, getContents)
import Data.Attoparsec.ByteString

import Lambia.Parse

main :: IO ()
main = do
  str <- getContents
  print $ parseOnly source str
