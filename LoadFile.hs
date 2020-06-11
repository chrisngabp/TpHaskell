module LoadFile where

import System.IO
import System.IO.Unsafe

loadFile :: FilePath -> String
loadFile = unsafePerformIO . readFile
