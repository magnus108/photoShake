module Utils.NoShit
    ( noShit
    ) where

import Utils.Free
import qualified Utils.Actions as A

myProgram :: A.TerminalM ()
myProgram = do
  m <- A.getLine 
  f <- A.readFile m 
  A.writeFile "bob.txt" f

noShit :: IO ()
noShit = A.interpret myProgram
