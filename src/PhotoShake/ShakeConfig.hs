module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    , getDumpFiles
    , getDumpDir
    , getDumpConfig
    , getDagsdatoDir
    , getDoneshootingDir
    , getLocationFile
    ) where


import Development.Shake.Config

import System.FilePath
import System.Directory

import qualified Data.HashMap.Lazy as HM

import PhotoShake.ShakeError
import PhotoShake.Shooting

import Control.Exception


data ShakeConfig = ShakeConfig 
    { _dumpConfig :: FilePath
    , _doneshootingConfig :: FilePath
    , _dagsdatoConfig:: FilePath
    , _locationConfig :: FilePath

    , _shootingType :: Shooting
    }


-- concider moving this or getting rid of it
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


getDumpConfig :: HM.HashMap String String -> FilePath
getDumpConfig config = case (HM.lookup "dumpConfig" config) of
    Nothing -> throw ConfigDumpMissing
    Just x -> x 


getDoneshootingConfig :: HM.HashMap String String -> FilePath
getDoneshootingConfig config = case (HM.lookup "doneshootingConfig" config) of
    Nothing -> error "wtf" --throw ConfigDoneshootingMissing
    Just x -> x


getDoneshootingDir :: FilePath -> IO FilePath
getDoneshootingDir x = do
    doneshootingConfig <- readConfigFile x `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
    case (HM.lookup "location" doneshootingConfig) of
        Nothing -> throw DoneshootingConfigFileMissing -- Same error
        Just y -> return y


getDagsdatoConfig :: HM.HashMap String String -> FilePath
getDagsdatoConfig config = case (HM.lookup "dagsdatoConfig" config) of
    Nothing -> throw ConfigDagsdatoMissing
    Just x -> x

    
getDagsdatoDir :: FilePath -> IO FilePath
getDagsdatoDir x = do
    dagsdatoConfig <- readConfigFile x `catchAny` (\_ -> throw DagsdatoConfigFileMissing)
    case (HM.lookup "location" dagsdatoConfig) of
        Nothing -> throw DagsdatoConfigFileMissing -- Same error
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
getLocationConfig config = case (HM.lookup "locationConfig" config) of
    Nothing -> throw ConfigLocationMissing
    Just x -> x



getDumpDir :: FilePath -> IO FilePath
getDumpDir x = do
    dumpConfig <- readConfigFile x `catchAny` (\_ -> throw DumpConfigFileMissing)
    case (HM.lookup "location" dumpConfig) of
        Nothing -> throw DumpConfigFileMissing -- Same error 
        Just y -> return y


getDumpFiles :: FilePath -> IO [FilePath]
getDumpFiles dumpDir = do
     files <- listDirectory dumpDir 
     return $ fmap (\x -> dumpDir </> x) files -- could be nicer


getLocationFile :: FilePath -> IO FilePath
getLocationFile x = do
    locationConfig <- readConfigFile x `catchAny` (\_ -> throw LocationConfigFileMissing)
    case (HM.lookup "location" locationConfig) of
        Nothing -> throw LocationConfigFileMissing -- Same error 
        Just y -> return y



-- could be better
toShakeConfig :: FilePath -> IO ShakeConfig
toShakeConfig x = do
    config <- readConfigFile x `catchAny` (\_ -> throw ConfigMissing)
    let dumpConfig = getDumpConfig config
    let doneshootingConfig = getDoneshootingConfig config
    let dagsdatoConfig = getDagsdatoConfig config
    let locationConfig = getLocationConfig config

    shootingType <- getShootingType config

    return $ ShakeConfig { _dumpConfig = dumpConfig
                         , _doneshootingConfig = doneshootingConfig
                         , _dagsdatoConfig = dagsdatoConfig
                         , _locationConfig = locationConfig
                         , _shootingType = shootingType
                         } 
