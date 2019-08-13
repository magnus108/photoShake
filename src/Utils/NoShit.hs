module Utils.NoShit
    ( noShit
    ) where

import Utils.Free
import qualified Utils.Actions as A

myProgram :: A.TerminalM ()
myProgram = do
  a <- A.getLine
  b <- A.getLine
  A.printLine (a ++ b)

noShit :: IO ()
noShit = A.interpret myProgram
