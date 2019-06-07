module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , catchAny
    , getDumpFiles
    , getDump
    , setLocation
    , setDump
    , setShooting
    , setSession
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
    , importSessions
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

import PhotoShake.Location
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


getPhotographerConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getPhotographerConfig root config = case (HM.lookup "photographerConfig" config) of
    Nothing -> throw ConfigPhotographerMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


getDumpConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getDumpConfig root config = case (HM.lookup "dumpConfig" config) of
    Nothing -> throw ConfigDumpMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


getDoneshootingConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getDoneshootingConfig root config = case (HM.lookup "doneshootingConfig" config) of
    Nothing -> throw ConfigDoneshootingMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


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
setSession:: ShakeConfig -> Sessions -> IO ()
setSession config sessions = do
    let filepath = _sessionConfig config
    writeFile filepath (encode sessions) `catchAny` (\_ -> throw SessionsConfigFileMissing)


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

--
-- does not really belong in this project
importSessions :: ShakeConfig -> FilePath -> IO ()
importSessions config fromFilePath = do
    let toFilePath = _sessionConfig config
    newSessions <- readFile fromFilePath `catchAny` (\_ -> throw SessionsConfigFileMissing)
    let sessions = decode newSessions :: Maybe Sessions
    case sessions of
            Nothing -> throw SessionsConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw SessionsConfigFileMissing)

importShootings :: ShakeConfig -> FilePath -> IO ()
importShootings config fromFilePath = do
    let toFilePath = _shootingsConfig config
    newShootings <- readFile fromFilePath `catchAny` (\_ -> throw ShootingConfigFileMissing)
    let shootings = decode newShootings :: Maybe Shootings
    case shootings of
            Nothing -> throw ShootingConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw ShootingConfigFileMissing)
    

getDagsdatoConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getDagsdatoConfig root config = case (HM.lookup "dagsdatoConfig" config) of
    Nothing -> throw ConfigDagsdatoMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x

    
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


setLocation :: ShakeConfig -> Location -> IO ()
setLocation config location = do
    let filepath = _locationConfig config
    writeFile filepath (encode location) `catchAny` (\_ -> throw LocationConfigFileMissing)


getShootingsConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getShootingsConfig root config = case (HM.lookup "shootingConfig" config) of
    Nothing -> throw ConfigShootingMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


getSessionConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getSessionConfig root config = case (HM.lookup "sessionConfig" config) of
    Nothing -> throw ConfigSessionMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


getShootings :: ShakeConfig -> IO Shootings
getShootings config = do
        let filepath = _shootingsConfig config
        shootingConfig <- readFile filepath `catchAny` (\_ -> throw ShootingConfigFileMissing) 
        let shootings = decode shootingConfig :: Maybe Shootings
        case shootings of
                Nothing -> throw ShootingConfigFileMissing
                Just y -> return y


getSessions :: ShakeConfig -> IO Sessions
getSessions config = do
        let filepath = _sessionConfig config
        sessionConfig <- readFile filepath `catchAny` (\_ -> throw SessionsConfigFileMissing) 
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
        case x of 
            NoPhotographers -> throw PhotographersConfigFileMissing
            Photographers y -> return (focus y)
            

getShooting :: ShakeConfig -> IO Shooting
getShooting config = do
        x <- getShootings config
        case x of 
            NoShootings -> throw ShootingConfigFileMissing
            Shootings y -> return (focus y)            


getSession :: ShakeConfig -> IO Session
getSession config = do
        x <- getSessions config
        case x of 
            NoSessions -> throw SessionsConfigFileMissing
            Sessions y -> return (focus y)


getLocationConfig :: Maybe FilePath -> HM.HashMap String String -> String
getLocationConfig root config = case (HM.lookup "locationConfig" config) of
    Nothing -> throw ConfigLocationMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x



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
    case dump of 
        NoDump -> throw DumpConfigFileMissing
        Dump x -> do
            files <- listDirectory x
            return $ fmap (\y -> x </> y) files -- could be nicer


getLocationFile :: ShakeConfig -> IO Location
getLocationFile config = do
    let filepath = _locationConfig config
    locationConfig <- readFile filepath `catchAny` (\_ -> throw LocationConfigFileMissing)
    let location = decode locationConfig :: Maybe Location
    case location of
        Nothing -> throw LocationConfigFileMissing -- Same error 
        Just y -> return y


-- could be better
toShakeConfig :: Maybe FilePath -> FilePath -> IO ShakeConfig
toShakeConfig root cfg = do 
    --bads 
    let path = case root of
            Nothing -> cfg
            Just x -> x </> cfg
    --bads 
    config <- readConfigFile path `catchAny` (\_ -> throw ConfigMissing)
    let dumpConfig = getDumpConfig root config
    let doneshootingConfig = getDoneshootingConfig root config
    let dagsdatoConfig = getDagsdatoConfig root config
    let locationConfig = getLocationConfig root config
    let shootingsConfig = getShootingsConfig root config
    let sessionConfig = getSessionConfig root config
    let photographerConfig = getPhotographerConfig root config

    return $ ShakeConfig { _dumpConfig = dumpConfig
                         , _doneshootingConfig = doneshootingConfig
                         , _dagsdatoConfig = dagsdatoConfig
                         , _locationConfig = locationConfig
                         , _shootingsConfig = shootingsConfig
                         , _sessionConfig = sessionConfig
                         , _photographerConfig = photographerConfig
                         } 
