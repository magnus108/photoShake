module Utils.NoShit
    ( noShit
    ) where

import Utils.Free
import qualified Utils.Actions as A

import Data.ByteString.Lazy.UTF8 as BLU 

myProgram :: A.TerminalM ()
myProgram = do
  A.writeFile "bob.txt" $ BLU.fromString "what"

noShit :: IO ()
noShit = A.interpret myProgram
