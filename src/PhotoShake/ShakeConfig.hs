module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    , getDumpFiles
    , getDumpDir
    , getDumpConfig
    , getDagsdatoDir
    , getDoneshootingDir
    , getShootings
    , getSessions
    , getPhotographers
    , getLocationFile
    ) where

import Prelude hiding (readFile)


import Development.Shake.Config

import System.FilePath
import System.Directory

import qualified Data.HashMap.Lazy as HM

import PhotoShake.ShakeError
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.Photographer

import Control.Exception

import Data.Aeson
import Data.ByteString.Lazy (readFile)


data ShakeConfig = ShakeConfig 
    { _dumpConfig :: FilePath
    , _doneshootingConfig :: FilePath
    , _dagsdatoConfig:: FilePath
    , _locationConfig :: FilePath
    , _shootingsConfig :: FilePath
    , _sessionConfig :: FilePath
    , _photographerConfig :: FilePath
    }


-- concider moving this or getting rid of it
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


getPhotographerConfig :: HM.HashMap String String -> FilePath
getPhotographerConfig config = case (HM.lookup "photographerConfig" config) of
    Nothing -> throw ConfigPhotographerMissing
    Just x -> x 


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


getShootingsConfig :: HM.HashMap String String -> FilePath
getShootingsConfig config = case (HM.lookup "shootingConfig" config) of
    Nothing -> throw ConfigShootingMissing
    Just x -> x


getSessionConfig :: HM.HashMap String String -> FilePath
getSessionConfig config = case (HM.lookup "sessionConfig" config) of
    Nothing -> throw ConfigSessionMissing
    Just x -> x


getShootings :: FilePath -> IO Shootings
getShootings x = do
        shootingConfig <- readFile x `catchAny` (\_ -> throw ShootingConfigFileMissing) 
        let shootings = decode shootingConfig :: Maybe Shootings
        case shootings of
                Nothing -> throw ShootingConfigFileMissing
                Just y -> return y


getSessions :: FilePath -> IO Sessions
getSessions x = do
        sessionConfig <- readFile x `catchAny` (\_ -> throw SessionsConfigFileMissing) 
        let sessions = decode sessionConfig :: Maybe Sessions
        case sessions of
                Nothing -> throw SessionsConfigParseError
                Just y -> return y



getPhotographers :: FilePath -> IO Photographers
getPhotographers x = do
        photographerConfig <- readFile x `catchAny` (\_ -> throw PhotographersConfigFileMissing) 
        let photographers = decode photographerConfig :: Maybe Photographers
        case photographers of
                Nothing -> throw PhotographersConfigFileMissing
                Just y -> return y
            

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
    let shootingsConfig = getShootingsConfig config
    let sessionConfig = getSessionConfig config
    let photographerConfig = getPhotographerConfig config

    return $ ShakeConfig { _dumpConfig = dumpConfig
                         , _doneshootingConfig = doneshootingConfig
                         , _dagsdatoConfig = dagsdatoConfig
                         , _locationConfig = locationConfig
                         , _shootingsConfig = shootingsConfig
                         , _sessionConfig = sessionConfig
                         , _photographerConfig = photographerConfig
                         } 
