module Utils.NoShit
  --  ( noShit
  --  )
  where

import Utils.Free
import qualified Utils.Actions as A


import PhotoShake.Photographer

myProgram :: A.TerminalM Photographers Photographers
myProgram = do
  z <- A.readFile "config/photographer.json"
  A.writeFile "config/photographer.json" z
  return z

noShit :: IO ()
noShit = do 
    x <- A.interpret myProgram
    putStrLn $ show x
