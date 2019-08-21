module Utils.NoShit
  --  ( noShit
  --  )
  where

import Utils.Free
import qualified Utils.Actions as A

import Data.ByteString.UTF8 as BU 


import PhotoShake.Photographer

myProgram :: A.TerminalM Photographers Photographers
myProgram = do
  A.writeFile "config/photographer.json" NoPhotographers
  A.readFile "config/photographer.json"

noShit :: IO ()
noShit = do 
    x <- A.interpret myProgram
    putStrLn $ show x
