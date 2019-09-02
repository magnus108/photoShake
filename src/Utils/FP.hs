module Utils.FP
    ( start
    , FP
    , fp
    , toFilePath
    , unFP
    , combine
    , mkFP
    )
  where

import Utils.Comonad
import Utils.Env

import System.FilePath ((</>))

newtype FP = FP { unFP :: Env FilePath FilePath }

fp :: Env FilePath FilePath -> FP
fp = FP


start :: FilePath -> Env FilePath FilePath
start x = env x x

combine :: FilePath -> Env FilePath FilePath -> FilePath
combine n x = extract x </> n

mkFP :: FilePath -> FilePath -> FP
mkFP root file = FP $ start root =>> combine file

toFilePath :: FP -> FilePath
toFilePath = extract . unFP
