module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    ) where

import Development.Shake
import Development.Shake.Config

import qualified Data.HashMap.Lazy as HM

import PhotoShake.ShakeError

import Control.Exception


data ShakeConfig = ShakeConfig 
    { _dumpFiles :: [FilePath]
    , _outDir :: FilePath
    , _location :: FilePath
    }


-- concider moving this or getting rid of it
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


getDumpDir :: HM.HashMap String String -> IO String
getDumpDir config = case (HM.lookup "dumpConfig" config) of
    Nothing -> throw ConfigDumpMissing
    Just x -> readFile x `catchAny` (\_ -> throw DumpConfigFileMissing)

getOutDir :: HM.HashMap String String -> String
getOutDir config = case (HM.lookup "outDir" config) of
    Nothing -> throw ConfigOutMissing
    Just x -> x


getLocationConfig :: HM.HashMap String String -> String
getLocationConfig config = case (HM.lookup "location" config) of
    Nothing -> throw ConfigLocationMissing
    Just x -> x


getDumpFiles :: FilePath -> IO [String]
getDumpFiles dumpDir = getDirectoryFilesIO "" [dumpDir ++ "/*"]
    `catchAny` (\_ -> throw DumpMissing)


-- could be better
toShakeConfig :: FilePath -> IO ShakeConfig
toShakeConfig x = do
    config <- readConfigFile x `catchAny` (\_ -> throw ConfigMissing)
    dumpDir <- getDumpDir config
    dumpFiles <- getDumpFiles dumpDir 
    let outDir = getOutDir config
    let location = getLocationConfig config
    return $ ShakeConfig { _dumpFiles = dumpFiles
                         , _outDir = outDir
                         , _location = location
                         } 
