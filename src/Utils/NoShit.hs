module Utils.NoShit
  --  ( noShit
  --  )
  where

import Utils.Free
import Utils.Env
import Utils.Comonad
import Utils.FP
import qualified Utils.Actions as A


import PhotoShake.Photographer


myProgram :: A.TerminalM Photographers Photographers
myProgram = do
  z <- A.readFile (mkFP "" "config/photographer.json")
  A.writeFile (mkFP "" "config/photographer.json") z
  return z

noShit :: IO ()
noShit = do 
    x <- A.interpret myProgram
    putStrLn $ show x

