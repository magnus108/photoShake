module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    , getDumpFiles
    , getDump
    , getDumpConfig
    , getDagsdato
    , getDoneshooting
    , getShootings
    , getSessions
    , getPhotographers
    , getLocationFile
    , getPhotographer 
    , getShooting
    , getSession
    ) where

import Prelude hiding (readFile)


import Development.Shake.Config

import System.FilePath
import System.Directory

import qualified Data.HashMap.Lazy as HM

import PhotoShake.Dagsdato
import PhotoShake.Doneshooting
import PhotoShake.Dump
import PhotoShake.ShakeError
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.Photographer

import Control.Exception

import Data.Aeson
import Data.ByteString.Lazy (readFile)

import Utils.ListZipper

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
    Nothing -> throw ConfigDoneshootingMissing
    Just x -> x


getDoneshooting :: ShakeConfig -> IO Doneshooting
getDoneshooting config = do
    let filepath = _doneshootingConfig config
    doneshootingConfig <- readFile filepath `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
    let doneshooting = decode doneshootingConfig :: Maybe Doneshooting
    case doneshooting of
            Nothing -> throw DoneshootingConfigFileMissing -- Same error
            Just y -> return y


getDagsdatoConfig :: HM.HashMap String String -> FilePath
getDagsdatoConfig config = case (HM.lookup "dagsdatoConfig" config) of
    Nothing -> throw ConfigDagsdatoMissing
    Just x -> x

    
getDagsdato :: ShakeConfig -> IO Dagsdato
getDagsdato config = do
    let filepath = _dagsdatoConfig config
    dagsdatoConfig <- readFile filepath `catchAny` (\_ -> throw DagsdatoConfigFileMissing)
    let dagsdato = decode dagsdatoConfig :: Maybe Dagsdato
    case dagsdato of
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


getPhotographer :: ShakeConfig -> IO Photographer
getPhotographer config = do
        let photographerConfig = _photographerConfig config
        x <- getPhotographers photographerConfig
        return (focus (unPhotographers x))
            

getShooting :: ShakeConfig -> IO Shooting
getShooting config = do
        let shootingConfig = _shootingsConfig config
        x <- getShootings shootingConfig
        return (focus (unShootings x))


getSession :: ShakeConfig -> IO Session
getSession config = do
        let sessionConfig = _sessionConfig config
        x <- getSessions sessionConfig
        return (focus (unSessions x))


getLocationConfig :: HM.HashMap String String -> String
getLocationConfig config = case (HM.lookup "locationConfig" config) of
    Nothing -> throw ConfigLocationMissing
    Just x -> x


getDump :: FilePath -> IO Dump
getDump x = do
    dumpConfig <- readFile x `catchAny` (\_ -> throw DumpConfigFileMissing)
    let dumpDir = decode dumpConfig :: Maybe Dump
    case dumpDir of
            Nothing -> throw DumpConfigFileMissing
            Just y -> return y


getDumpFiles :: ShakeConfig -> IO [FilePath]
getDumpFiles config = do
    let dumpConfig = _dumpConfig config
    dump <- getDump dumpConfig
    files <- listDirectory (unDump dump)
    return $ fmap (\x -> (unDump dump) </> x) files -- could be nicer


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
