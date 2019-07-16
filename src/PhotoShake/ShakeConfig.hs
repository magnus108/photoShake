module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , toShakeConfig
    , setGrades
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
    , getBuilt
    , setBuilt
    , setBuilt'
    , getGrades
    ) where

import Prelude hiding (readFile, writeFile, length)

import Data.Vector (Vector, find, (++), fromList, toList)

import Development.Shake.Config
import Data.List hiding (length)
import Data.Csv hiding (decode, encode)

import System.FilePath
import System.Directory

import qualified Data.HashMap.Lazy as HM


import PhotoShake.Built
import PhotoShake.Location
import PhotoShake.Dagsdato
import PhotoShake.Photographee
import PhotoShake.Doneshooting
import PhotoShake.Dump
import PhotoShake.ShakeError
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.Photographer

import Control.Exception

import Data.Aeson
import Data.ByteString.Lazy (readFile, writeFile,  length)

import Utils.ListZipper hiding (toList)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString, toString)

data ShakeConfig = ShakeConfig 
    { _dumpConfig :: FilePath
    , _doneshootingConfig :: FilePath
    , _dagsdatoConfig:: FilePath
    , _locationConfig :: FilePath
    , _shootingsConfig :: FilePath
    , _sessionConfig :: FilePath
    , _photographerConfig :: FilePath
    , _builtConfig :: FilePath
    , _gradeConfig :: FilePath
    }


-- concider moving this or getting rid of it
catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch


getBuiltConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getBuiltConfig root config = case (HM.lookup "builtConfig" config) of
    Nothing -> throw ConfigBuiltMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


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

setGrades :: ShakeConfig -> Grades -> IO ()
setGrades config grades = do
    let filepath = _gradeConfig config
    gradeConfig <- readFile filepath `catchAny` (\_ -> throw GradeConfigFileMissing)
    let grades' = case grades of
            NoGrades -> NoGrades
            Grades (ListZipper ls x rs) -> Grades $ 
                ListZipper (filter (\zz -> zz /= x) $ filter (\zz -> zz `notElem` rs) $ nub ls) x (filter (\zz -> zz /= x) $ nub rs)
    seq (length gradeConfig) (writeFile filepath (encode grades') `catchAny` (\_ -> throw GradeConfigFileMissing))

getDoneshooting :: ShakeConfig -> IO Doneshooting
getDoneshooting config = do
    let filepath = _doneshootingConfig config
    doneshootingConfig <- readFile filepath `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
    seq (length doneshootingConfig) (return ())
    let doneshooting = decode doneshootingConfig :: Maybe Doneshooting
    case doneshooting of
            Nothing -> throw DoneshootingConfigFileMissing -- Same error
            Just y -> return y

-- ikke en rigtig setter mere en der skriver
setBuilt:: ShakeConfig -> String -> Photographee -> IO ()
setBuilt config s photographee = do
    -- there is a smarter way of doing this
    let b = case s of
            "" -> NoBuilt
            x -> case words x of
                "Finished":_ -> Built photographee x
                _ -> Building photographee x

    let filepath = _builtConfig config
    builtConfig <- readFile filepath `catchAny` (\_ -> throw BuiltConfigFileMissing)
    seq (length builtConfig) (writeFile filepath (encode b) `catchAny` (\_ -> throw BuiltConfigFileMissing))


--dont use
setBuilt' :: ShakeConfig -> Built -> IO ()
setBuilt' config built = do
    let filepath = _builtConfig config
    builtConfig <- readFile filepath `catchAny` (\_ -> throw BuiltConfigFileMissing)
    seq (length builtConfig) (writeFile filepath (encode built) `catchAny` (\_ -> throw BuiltConfigFileMissing))



getGrades :: ShakeConfig -> IO Grades
getGrades config = do
        let filepath = _gradeConfig config
        gradesConfig <- readFile filepath `catchAny` (\_ -> throw GradeConfigFileMissing) 
        seq (length gradesConfig) (return ())
        let grades = decode gradesConfig :: Maybe Grades
        case grades of
                Nothing -> throw GradeConfigFileMissing
                Just y -> return y



-- ikke en rigtig setter mere en der skriver
setDoneshooting :: ShakeConfig -> Doneshooting -> IO ()
setDoneshooting config doneshooting = do
    let filepath = _doneshootingConfig config
    doneshootings <- readFile filepath `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
    seq (length doneshootings) (writeFile filepath (encode doneshooting) `catchAny` (\_ -> throw DoneshootingConfigFileMissing))
--
-- ikke en rigtig setter mere en der skriver
setShooting:: ShakeConfig -> Shootings -> IO ()
setShooting config shootings = do
    let filepath = _shootingsConfig config
    shooting <- readFile filepath `catchAny` (\_ -> throw ShootingConfigFileMissing)
    seq (length shooting) (writeFile filepath (encode shootings) `catchAny` (\_ -> throw ShootingConfigFileMissing))


-- ikke en rigtig setter mere en der skriver
setSession:: ShakeConfig -> Sessions -> IO ()
setSession config sessions = do
    let filepath = _sessionConfig config
    session <- readFile filepath `catchAny` (\_ -> throw SessionsConfigFileMissing)
    seq (length session) (writeFile filepath (encode sessions) `catchAny` (\_ -> throw SessionsConfigFileMissing))


-- ikke en rigtig setter mere en der skriver
-- does not really belong in this project
setPhotographers :: ShakeConfig -> Photographers -> IO ()
setPhotographers config photographers = do
    let filepath = _photographerConfig config
    photographers' <- readFile filepath `catchAny` (\_ -> throw PhotographersConfigFileMissing)
    seq (length photographers') (writeFile filepath (encode photographers) `catchAny` (\_ -> throw PhotographersConfigFileMissing))

--
-- does not really belong in this project
importPhotographers :: ShakeConfig -> FilePath -> IO ()
importPhotographers config fromFilePath = do
    let toFilePath = _photographerConfig config
    newPhotographers <- readFile fromFilePath `catchAny` (\_ -> throw PhotographersConfigFileMissing)
    let photographers = decode newPhotographers :: Maybe Photographers
    seq (length newPhotographers) (case photographers of
            Nothing -> throw PhotographersConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw PhotographersConfigFileMissing))

--
-- does not really belong in this project
importSessions :: ShakeConfig -> FilePath -> IO ()
importSessions config fromFilePath = do
    let toFilePath = _sessionConfig config
    newSessions <- readFile fromFilePath `catchAny` (\_ -> throw SessionsConfigFileMissing)
    let sessions = decode newSessions :: Maybe Sessions
    seq (length newSessions) (case sessions of
            Nothing -> throw SessionsConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw SessionsConfigFileMissing))

importShootings :: ShakeConfig -> FilePath -> IO ()
importShootings config fromFilePath = do
    let toFilePath = _shootingsConfig config
    newShootings <- readFile fromFilePath `catchAny` (\_ -> throw ShootingConfigFileMissing)
    let shootings = decode newShootings :: Maybe Shootings
    seq (length newShootings) (case shootings of
            Nothing -> throw ShootingConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw ShootingConfigFileMissing))
    

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
    seq (length dagsdatoConfig) (case dagsdato of
        Nothing -> throw DagsdatoConfigFileMissing -- Same error
        Just y -> return y)


setDagsdato :: ShakeConfig -> Dagsdato -> IO ()
setDagsdato config dagsdato = do
    let filepath = _dagsdatoConfig config
    dagsdatoConfig <- readFile filepath `catchAny` (\_ -> throw DagsdatoConfigFileMissing)
    seq (length dagsdatoConfig) (writeFile filepath (encode dagsdato) `catchAny` (\_ -> throw DagsdatoConfigFileMissing))


setLocation :: ShakeConfig -> Location -> IO ()
setLocation config location = do
    let filepath = _locationConfig config
    locationConfig <- readFile filepath `catchAny` (\_ -> throw LocationConfigFileMissing)
    seq (length locationConfig) (writeFile filepath (encode location) `catchAny` (\_ -> throw LocationConfigFileMissing))
    error $ show locationConfig
    grades <- parseGrades $ toString locationConfig
    setGrades config grades


getShootingsConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getShootingsConfig root config = case (HM.lookup "shootingConfig" config) of
    Nothing -> throw ConfigShootingMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x

getGradeConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getGradeConfig root config = case (HM.lookup "gradeConfig" config) of
    Nothing -> throw ConfigGradeMissing
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
        seq (length shootingConfig) (return ())
        case shootings of
                Nothing -> throw ShootingConfigFileMissing
                Just y -> return y


getSessions :: ShakeConfig -> IO Sessions
getSessions config = do
        let filepath = _sessionConfig config
        sessionConfig <- readFile filepath `catchAny` (\_ -> throw SessionsConfigFileMissing) 
        seq (length sessionConfig) (return ())
        let sessions = decode sessionConfig :: Maybe Sessions
        case sessions of
                Nothing -> throw SessionsConfigParseError
                Just y -> return y



getPhotographers :: ShakeConfig -> IO Photographers
getPhotographers config = do
        let filepath = _photographerConfig config
        photographerConfig <- readFile filepath `catchAny` (\_ -> throw PhotographersConfigFileMissing) 
        seq (length photographerConfig) (return ())
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
    seq (length dumpConfig) (return ())
    let dumpDir = decode dumpConfig :: Maybe Dump
    case dumpDir of
            Nothing -> throw DumpConfigFileMissing
            Just y -> return y


getBuilt :: ShakeConfig -> IO Built
getBuilt config = do
    let filepath = _builtConfig config
    builtConfig <- readFile filepath `catchAny` (\_ -> throw BuiltConfigFileMissing)
    seq (length builtConfig) (return ())
    let built = decode builtConfig :: Maybe Built
    case built of
            Nothing -> throw BuiltConfigFileMissing
            Just y -> return y


setDump :: ShakeConfig -> Dump -> IO ()
setDump config dump = do
    let filepath = _dumpConfig config
    dumps <- readFile filepath `catchAny` (\_ -> throw DumpConfigFileMissing)
    seq (length dumps) (writeFile filepath (encode dump) `catchAny` (\_ -> throw DumpConfigFileMissing))


getDumpFiles :: ShakeConfig -> IO [(FilePath, FilePath)]
getDumpFiles config = do
    dump <- getDump config
    case dump of 
        NoDump -> return []
        Dump x -> do
            files <- listDirectory x `catchAny` (\_ -> throw DumpMissing)
            let files' = filter (isExtensionOf "CR2") files -- bad use
            return $ fmap (\y -> (x </> y, x </> y -<.> "JPG")) files' -- could be nicer


getLocationFile :: ShakeConfig -> IO Location
getLocationFile config = do
    let filepath = _locationConfig config
    locationConfig <- readFile filepath `catchAny` (\_ -> throw LocationConfigFileMissing)
    seq (length locationConfig) (return ())
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
    let builtConfig = getBuiltConfig root config
    let gradeConfig = getGradeConfig root config

    return $ ShakeConfig { _dumpConfig = dumpConfig
                         , _doneshootingConfig = doneshootingConfig
                         , _dagsdatoConfig = dagsdatoConfig
                         , _locationConfig = locationConfig
                         , _shootingsConfig = shootingsConfig
                         , _sessionConfig = sessionConfig
                         , _photographerConfig = photographerConfig
                         , _builtConfig = builtConfig
                         , _gradeConfig = gradeConfig
                         } 
