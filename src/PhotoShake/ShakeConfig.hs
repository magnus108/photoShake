module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    ) where


import Development.Shake.Config

import System.FilePath
import System.Directory

import qualified Data.HashMap.Lazy as HM

import PhotoShake.ShakeError
import PhotoShake.Shooting

import Control.Exception


data ShakeConfig = ShakeConfig 
    { _dumpFiles :: [FilePath]
    , _doneshootingDir :: FilePath
    , _dagsdatoDir :: FilePath
    , _shootingType :: Shooting
    , _location :: FilePath
    }


-- concider moving this or getting rid of it
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


getDumpDir :: HM.HashMap String String -> IO String
getDumpDir config = case (HM.lookup "dumpConfig" config) of
    Nothing -> throw ConfigDumpMissing
    Just x -> do
        dumpConfig <- readConfigFile x `catchAny` (\_ -> throw DumpConfigFileMissing)
        case (HM.lookup "location" dumpConfig) of
            Nothing -> throw DumpConfigFileMissing -- Same error as above
            Just y -> return y


getDoneshootingDir :: HM.HashMap String String -> IO String
getDoneshootingDir config = case (HM.lookup "doneshootingConfig" config) of
    Nothing -> error "wtf" --throw ConfigDoneshootingMissing
    Just x -> do
        doneshootingConfig <- readConfigFile x `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
        case (HM.lookup "location" doneshootingConfig) of
            Nothing -> throw DoneshootingConfigFileMissing -- Same error as above
            Just y -> return y

getDagsdatoDir :: HM.HashMap String String -> IO String
getDagsdatoDir config = case (HM.lookup "dagsdatoConfig" config) of
    Nothing -> throw ConfigDagsdatoMissing
    Just x -> do
        dagsdatoConfig <- readConfigFile x `catchAny` (\_ -> throw DagsdatoConfigFileMissing)
        case (HM.lookup "location" dagsdatoConfig) of
            Nothing -> throw DagsdatoConfigFileMissing -- Same error as above
            Just y -> return y


getShootingType :: HM.HashMap String String -> IO Shooting
getShootingType config = case (HM.lookup "shootingConfig" config) of
    Nothing -> throw ConfigShootingMissing
    Just x -> do
        shootingConfig <- readConfigFile x `catchAny` (\_ -> throw ShootingConfigFileMissing)
        case (HM.lookup "shooting" shootingConfig) of
            Nothing -> throw ShootingConfigFileMissing -- Same error as above
            Just y -> do
                case y of
                    "normal" -> return Normal
                    "omfoto" -> return Omfoto
                    _ -> throw ShootingConfigFileMissing


getLocationConfig :: HM.HashMap String String -> String
getLocationConfig config = case (HM.lookup "location" config) of
    Nothing -> throw ConfigLocationMissing
    Just x -> x


getDumpFiles :: FilePath -> IO [String]
getDumpFiles dumpDir = do
     files <- listDirectory dumpDir 
     return $ fmap (\x -> dumpDir </> x) files -- could be nicer

-- could be better
toShakeConfig :: FilePath -> IO ShakeConfig
toShakeConfig x = do
    config <- readConfigFile x `catchAny` (\_ -> throw ConfigMissing)
    dumpDir <- getDumpDir config
    dumpFiles <- getDumpFiles dumpDir 
    doneshootingDir <- getDoneshootingDir config
    dagsdatoDir <- getDagsdatoDir config

    shootingType <- getShootingType config

    let location = getLocationConfig config
    return $ ShakeConfig { _dumpFiles = dumpFiles
                         , _doneshootingDir = doneshootingDir
                         , _dagsdatoDir = dagsdatoDir
                         , _location = location
                         , _shootingType = shootingType
                         } 
