module PhotoShake.ShakeConfig
    ( ShakeConfig(..)
    , getIdSelection
    , setIdSelection
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
    , getDagsdatoBackup
    , setDagsdato
    , setDagsdatoBackup
    , setPhotographers
    , getDoneshooting
    , getDoneshootingBackup
    , setDoneshooting
    , setDoneshootingBackup
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
    , getGradeSelectionConfig
    , setGradeSelection
    , getGradeSelection
    ) where

import qualified PhotoShake.Id as Id
import qualified Utils.FP as FP
import qualified Utils.Actions as Actions

import Prelude hiding (readFile, writeFile, length)

import Data.Vector (Vector, find, (++), fromList, toList)

import Development.Shake.Config
import Data.List hiding (length)
import Data.Csv hiding (decode, encode)

import qualified PhotoShake.State as S

import System.FilePath
import System.Directory

import qualified Data.HashMap.Lazy as HM


import PhotoShake.Built
import qualified PhotoShake.Location as Location
import PhotoShake.Dagsdato
import PhotoShake.Photographee
import PhotoShake.Doneshooting hiding (getDoneshooting, setDoneshooting)
import PhotoShake.Dump
import PhotoShake.ShakeError
import qualified PhotoShake.Shooting as Shooting
import qualified PhotoShake.Session as Session
import qualified PhotoShake.Grade as Grade
import PhotoShake.Photographer hiding (getPhotographers, setPhotographers)

import Control.Exception

import Data.Aeson
import Data.ByteString.Lazy (readFile, writeFile,  length)

import Utils.ListZipper hiding (toList)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy.UTF8 (fromString, toString)

data ShakeConfig = ShakeConfig 
    { _dumpConfig :: FilePath
    , _doneshootingConfig :: FilePath
    , _doneshootingBackupConfig :: FilePath
    , _dagsdatoConfig:: FilePath
    , _dagsdatoBackupConfig:: FilePath
    , _locationConfig :: FilePath
    , _shootingsConfig :: FilePath
    , _sessionConfig :: FilePath
    , _photographerConfig :: FilePath
    , _builtConfig :: FilePath
    , _gradeConfig :: FilePath
    , _gradeSelectionConfig :: FilePath

    , _stateConfig :: FilePath
    , _idConfig :: FilePath
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


getIdConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getIdConfig root config = case (HM.lookup "idConfig" config) of
    Nothing -> throw IdConfigFileMissing 
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

getDoneshootingBackupConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getDoneshootingBackupConfig root config = case (HM.lookup "doneshootingBackupConfig" config) of
    Nothing -> throw ConfigDoneshootingMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x

setGrades :: ShakeConfig -> Grade.Grades -> IO ()
setGrades config grades_ = do
    let filepath = _gradeConfig config
    gradeConfig <- readFile filepath `catchAny` (\_ -> throw GradeConfigFileMissing)
    let grades' = Grade.grades Grade.noGrades (\(ListZipper ls x rs) -> Grade.yesGrades $ ListZipper (filter (\zz -> zz /= x) $ filter (\zz -> zz `notElem` rs) $ nub ls) x (filter (\zz -> zz /= x) $ nub rs)) grades_

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


getDoneshootingBackup :: ShakeConfig -> IO Doneshooting
getDoneshootingBackup config = do
    let filepath = _doneshootingBackupConfig config
    doneshootingConfig <- readFile filepath `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
    seq (length doneshootingConfig) (return ())
    let doneshooting = decode doneshootingConfig :: Maybe Doneshooting
    case doneshooting of
            Nothing -> error "fuck"-- DoneshootingConfigFileMissing -- Same error
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


setDoneshootingBackup :: ShakeConfig -> Doneshooting -> IO ()
setDoneshootingBackup config doneshooting = do
    let filepath = _doneshootingBackupConfig config
    doneshootings <- readFile filepath `catchAny` (\_ -> throw DoneshootingConfigFileMissing)
    seq (length doneshootings) (writeFile filepath (encode doneshooting) `catchAny` (\_ -> throw DoneshootingConfigFileMissing))
--
-- ikke en rigtig setter mere en der skriver
setShooting:: ShakeConfig -> Shooting.Shootings -> IO ()
setShooting config shootings = do
    let filepath = _shootingsConfig config
    shooting <- readFile filepath `catchAny` (\_ -> throw ShootingConfigFileMissing)
    seq (length shooting) (writeFile filepath (encode shootings) `catchAny` (\_ -> throw ShootingConfigFileMissing))


-- ikke en rigtig setter mere en der skriver
setSession:: ShakeConfig -> Session.Sessions -> IO ()
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
    let sessions = decode newSessions :: Maybe Session.Sessions
    seq (length newSessions) (case sessions of
            Nothing -> throw SessionsConfigFileMissing -- Same error
            Just y -> do
                writeFile toFilePath (encode y) `catchAny` (\_ -> throw SessionsConfigFileMissing))

importShootings :: ShakeConfig -> FilePath -> IO ()
importShootings config fromFilePath = do
    let toFilePath = _shootingsConfig config
    newShootings <- readFile fromFilePath `catchAny` (\_ -> throw ShootingConfigFileMissing)
    let shootings = decode newShootings :: Maybe Shooting.Shootings
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

getDagsdatoBackupConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getDagsdatoBackupConfig root config = case (HM.lookup "dagsdatoBackupConfig" config) of
    Nothing -> error "what" --ConfigDagsdatoMissing
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

getDagsdatoBackup :: ShakeConfig -> IO Dagsdato
getDagsdatoBackup config = do
    let filepath = _dagsdatoBackupConfig config
    dagsdatoConfig <- readFile filepath `catchAny` (\_ -> error "fuck")
    let dagsdato = decode dagsdatoConfig :: Maybe Dagsdato
    seq (length dagsdatoConfig) (case dagsdato of
        Nothing -> error "damit" --DagsdatoConfigFileMissing -- Same error
        Just y -> return y)

setDagsdato :: ShakeConfig -> Dagsdato -> IO ()
setDagsdato config dagsdato = do
    let filepath = _dagsdatoConfig config
    dagsdatoConfig <- readFile filepath `catchAny` (\_ -> throw DagsdatoConfigFileMissing)
    seq (length dagsdatoConfig) (writeFile filepath (encode dagsdato) `catchAny` (\_ -> throw DagsdatoConfigFileMissing))

setDagsdatoBackup :: ShakeConfig -> Dagsdato -> IO ()
setDagsdatoBackup config dagsdato = do
    let filepath = _dagsdatoBackupConfig config
    dagsdatoConfig <- readFile filepath `catchAny` (\_ -> throw DagsdatoConfigFileMissing)
    seq (length dagsdatoConfig) (writeFile filepath (encode dagsdato) `catchAny` (\_ -> throw DagsdatoConfigFileMissing))


setGradeSelection :: ShakeConfig -> GradeSelection -> IO ()
setGradeSelection config grade = do
    let filepath = _gradeSelectionConfig config
    config <- readFile filepath `catchAny` (\_ -> throw GradeConfigFileMissing)
    seq (length config) (writeFile filepath (encode grade) `catchAny` (\_ -> throw GradeConfigFileMissing))


setIdSelection :: ShakeConfig -> Id.Id -> IO ()
setIdSelection config x = do
    let filepath = _idConfig config
    Actions.interpret (Id.setId (FP.fp (FP.start filepath)) x)



setLocation :: ShakeConfig -> Location.Location -> IO ()
setLocation config xxx = do
    let filepath = _locationConfig config
    locationConfig <- readFile filepath `catchAny` (\_ -> throw LocationConfigFileMissing)
    seq (length locationConfig) (writeFile filepath (encode xxx) `catchAny` (\_ -> throw LocationConfigFileMissing))
    Location.location (setGrades config $ Grade.noGrades) (\ f -> do
            grades <- parseGrades f
            setGrades config grades) xxx 


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

getStateConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getStateConfig root config = case (HM.lookup "stateConfig" config) of
    Nothing -> throw ConfigGradeMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x

getGradeSelectionConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getGradeSelectionConfig root config = case (HM.lookup "gradeSelectionConfig" config) of
    Nothing -> throw ConfigGradeMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x

getGradeSelection :: ShakeConfig -> IO GradeSelection
getGradeSelection config = do
        let filepath = _gradeSelectionConfig config
        config <- readFile filepath `catchAny` (\_ -> error "lol") 
        let grade = decode $ config :: Maybe GradeSelection
        seq (length config) (return ())
        case grade of
                Nothing -> throw ConfigGradeMissing
                Just y -> return $ y

getIdSelection :: ShakeConfig -> IO Id.Id
getIdSelection config = do
        let filepath = _idConfig config
        Actions.interpret (Id.getId (FP.fp (FP.start filepath)))


getSessionConfig :: Maybe FilePath -> HM.HashMap String String -> FilePath
getSessionConfig root config = case (HM.lookup "sessionConfig" config) of
    Nothing -> throw ConfigSessionMissing
    Just x -> case root of 
        Nothing -> x 
        Just y -> y </> x


getShootings :: ShakeConfig -> IO Shooting.Shootings
getShootings config = do
        let filepath = _shootingsConfig config
        shootingConfig <- readFile filepath `catchAny` (\_ -> throw ShootingConfigFileMissing) 
        let shootings = decode shootingConfig :: Maybe Shooting.Shootings
        seq (length shootingConfig) (return ())
        case shootings of
                Nothing -> throw ShootingConfigFileMissing
                Just y -> return y


getSessions :: ShakeConfig -> IO Session.Sessions
getSessions config = do
        let filepath = _sessionConfig config
        sessionConfig <- readFile filepath `catchAny` (\_ -> throw SessionsConfigFileMissing) 
        seq (length sessionConfig) (return ())
        let sessions = decode sessionConfig :: Maybe Session.Sessions
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
        photographers (throw PhotographersConfigFileMissing)
            (\y -> return (focus y)) x
            

getShooting :: ShakeConfig -> IO Shooting.Shooting
getShooting config = do
        x <- getShootings config
        Shooting.shootings (throw ShootingConfigFileMissing) (\y -> return (focus y)) x


getSession :: ShakeConfig -> IO Session.Session
getSession config = do
        x <- getSessions config
        Session.sessions (throw SessionsConfigFileMissing)
                (\y -> return (focus y)) x


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


getDumpFiles :: Dump -> IO [(FilePath, FilePath)]
getDumpFiles x = do
    dump (return []) (\x -> do
            files <- listDirectory x `catchAny` (\_ -> throw DumpMissing)
            let files' = filter (\z -> isExtensionOf "CR2" z || (isExtensionOf "cr2" z)) files -- bad use
            let files2' = filter (\z -> isExtensionOf "JPG" z || (isExtensionOf "jpg" z)) files -- bad use
            files'' <- mapM (\file -> do 
                    b <- (doesFileExist (x </> file -<.> "JPG")) 
                    b1 <- (doesFileExist (x </> file -<.> "jpg")) 
                    let b3 = all id $ fmap (\z -> elem (z -<.> "CR2") files' || (elem (z -<.> "cr2") files')) files2'
                    if ( b || b1 ) && b3 then return file else throw JPGMissing ) files' -- this is error
            return $ fmap (\y -> (x </> y, x </> y -<.> "JPG")) files'' -- could be nicer
            ) x


getLocationFile :: ShakeConfig -> IO Location.Location
getLocationFile config = do
    let filepath = _locationConfig config
    locationConfig <- readFile filepath `catchAny` (\_ -> throw LocationConfigFileMissing)
    seq (length locationConfig) (return ())
    let location = decode locationConfig :: Maybe Location.Location
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
    let doneshootingBackupConfig = getDoneshootingBackupConfig root config

    let dagsdatoConfig = getDagsdatoConfig root config
    let dagsdatoBackupConfig = getDagsdatoBackupConfig root config

    let locationConfig = getLocationConfig root config
    let shootingsConfig = getShootingsConfig root config
    let sessionConfig = getSessionConfig root config
    let photographerConfig = getPhotographerConfig root config
    let builtConfig = getBuiltConfig root config
    let gradeConfig = getGradeConfig root config

    let idConfig = getIdConfig root config

    let gradeSelectionConfig = getGradeSelectionConfig root config

    let stateConfig = getStateConfig root config

    return $ ShakeConfig { _dumpConfig = dumpConfig
                         , _doneshootingConfig = doneshootingConfig
                         , _doneshootingBackupConfig = doneshootingBackupConfig
                         , _dagsdatoConfig = dagsdatoConfig
                         , _dagsdatoBackupConfig = dagsdatoBackupConfig
                         , _locationConfig = locationConfig
                         , _shootingsConfig = shootingsConfig
                         , _sessionConfig = sessionConfig
                         , _photographerConfig = photographerConfig
                         , _builtConfig = builtConfig
                         , _gradeConfig = gradeConfig
                         , _gradeSelectionConfig = gradeSelectionConfig
                         , _stateConfig = stateConfig
                         , _idConfig = idConfig
                         } 
