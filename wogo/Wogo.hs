-- 
-- Wogo.
--
--

import System.Environment
import WogoParser

-- working.
main :: IO ()
main = do args <- getArgs
          prettyLogoFromFile (args !! 0)
