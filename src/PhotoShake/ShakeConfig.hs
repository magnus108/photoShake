module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    , getDumpFiles
    , getDump
    , setDump
    , setShooting
    , getDumpConfig
    , getDagsdato
    , setDagsdato
    , setPhotographers
    , getDoneshooting
    , setDoneshooting
    , getShootings
    , getSessions
    , getPhotographers
    , importPhotographers
    , importShootings
    , getLocationFile
    , getPhotographer 
    , getShooting
    , getSession
    ) where

import Prelude hiding (readFile, writeFile)


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
import Data.ByteString.Lazy (readFile, writeFile)

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


-- ikke en rigtig setter mere en der skriver
setDoneshooting :: ShakeConfig -> Doneshooting -> IO ()
setDoneshooting config doneshooting = do
    let filepath = _doneshootingConfig config
    writeFile filepath (encode doneshooting) `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
--
-- ikke en rigtig setter mere en der skriver
setShooting:: ShakeConfig -> Shootings -> IO ()
setShooting config shootings = do
    let filepath = _shootingsConfig config
    writeFile filepath (encode shootings) `catchAny` (\_ -> throw ShootingConfigFileMissing)

-- ikke en rigtig setter mere en der skriver
-- does not really belong in this project
setPhotographers :: ShakeConfig -> Photographers -> IO ()
setPhotographers config photographers = do
    let filepath = _photographerConfig config
    writeFile filepath (encode photographers) `catchAny` (\_ -> throw PhotographersConfigFileMissing)

--
-- does not really belong in this project
importPhotographers :: ShakeConfig -> FilePath -> IO ()
importPhotographers config fromFilePath = do
    let toFilePath = _photographerConfig config
    newPhotographers <- readFile fromFilePath `catchAny` (\_ -> throw PhotographersConfigFileMissing)
    let photographers = decode newPhotographers :: Maybe Photographers
    case photographers of
            Nothing -> throw PhotographersConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw PhotographersConfigFileMissing)


importShootings :: ShakeConfig -> FilePath -> IO ()
importShootings config fromFilePath = do
    let toFilePath = _shootingsConfig config
    newShootings <- readFile fromFilePath `catchAny` (\_ -> throw ShootingConfigFileMissing)
    let shootings = decode newShootings :: Maybe Shootings
    case shootings of
            Nothing -> throw ShootingConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw ShootingConfigFileMissing)
    



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


setDagsdato :: ShakeConfig -> Dagsdato -> IO ()
setDagsdato config dagsdato = do
    let filepath = _dagsdatoConfig config
    writeFile filepath (encode dagsdato) `catchAny` (\_ -> throw DagsdatoConfigFileMissing)


getShootingsConfig :: HM.HashMap String String -> FilePath
getShootingsConfig config = case (HM.lookup "shootingConfig" config) of
    Nothing -> throw ConfigShootingMissing
    Just x -> x


getSessionConfig :: HM.HashMap String String -> FilePath
getSessionConfig config = case (HM.lookup "sessionConfig" config) of
    Nothing -> throw ConfigSessionMissing
    Just x -> x


getShootings :: ShakeConfig -> IO Shootings
getShootings config = do
        let filepath = _shootingsConfig config
        shootingConfig <- readFile filepath `catchAny` (\_ -> throw ShootingConfigFileMissing) 
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



getPhotographers :: ShakeConfig -> IO Photographers
getPhotographers config = do
        let filepath = _photographerConfig config
        photographerConfig <- readFile filepath `catchAny` (\_ -> throw PhotographersConfigFileMissing) 
        let photographers = decode photographerConfig :: Maybe Photographers
        case photographers of
                Nothing -> throw PhotographersConfigFileMissing
                Just y -> return y


getPhotographer :: ShakeConfig -> IO Photographer
getPhotographer config = do
        x <- getPhotographers config
        return (focus (unPhotographers x))
            

getShooting :: ShakeConfig -> IO Shooting
getShooting config = do
        x <- getShootings config
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


getDump :: ShakeConfig -> IO Dump
getDump config = do
    let filepath = _dumpConfig config
    dumpConfig <- readFile filepath `catchAny` (\_ -> throw DumpConfigFileMissing)
    let dumpDir = decode dumpConfig :: Maybe Dump
    case dumpDir of
            Nothing -> throw DumpConfigFileMissing
            Just y -> return y


setDump :: ShakeConfig -> Dump -> IO ()
setDump config dump = do
    let filepath = _dumpConfig config
    writeFile filepath (encode dump) `catchAny` (\_ -> throw DumpConfigFileMissing)


getDumpFiles :: ShakeConfig -> IO [FilePath]
getDumpFiles config = do
    dump <- getDump config
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
