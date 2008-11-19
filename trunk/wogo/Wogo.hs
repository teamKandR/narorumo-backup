-- 
-- Wogo.
--
--

import System.Environment

haqify s = "Haq! " ++ s

-- working.
main :: IO ()
main = do args <- getArgs

          input <- (readFile (args !! 0))
          putStr (haqify input)
