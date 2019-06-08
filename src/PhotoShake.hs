{-# LANGUAGE OverloadedStrings #-}
module PhotoShake
    ( entry
    , myShake
    , opts
    , mkDoneshootingPath
    , mkDagsdatoPath
    ) where

import Data.Strings


import Development.Shake hiding (Normal)
import Development.Shake.FilePath

import PhotoShake.ShakeConfig
import PhotoShake.Photographee

import PhotoShake.Location
import PhotoShake.Dagsdato
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session
import PhotoShake.ShakeError

import qualified PhotoShake.Photographer as PR


import Data.Time.Format
import Data.Time.Clock

import Data.List
import Data.List.Index



getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d" 



entry :: IO ()
entry = do
    config <- toShakeConfig Nothing "config.cfg"
    location <- getLocationFile config
    -- ehh
    -- can make error
    case location of
        NoLocation -> error "no location given"
        Location xxx -> do
            photographeeId <- getLine
            photographee <- findPhotographee xxx photographeeId 

            ---ehhh2
            time <- getCurrentTime
            myShake config photographee (takeBaseName xxx) time


shakeDir :: FilePath
shakeDir = "._build"


opts :: ShakeOptions
opts = shakeOptions { shakeFiles = shakeDir
                    , shakeProgress = progressSimple -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }


myShake :: ShakeConfig -> Photographee -> String -> UTCTime -> IO ()
myShake config photographee location time = shake opts $ actions config photographee location time


mkDoneshootingPath :: Doneshooting -> Photographee -> String -> PR.Photographer -> Session -> Shooting -> String -> Int -> FilePath
mkDoneshootingPath NoDoneshooting _ _ _ _ _ _ _ = throw ConfigDoneshootingMissing
mkDoneshootingPath (Doneshooting doneshootingDir) photographee location photographer session shooting filename index = doneshootingDir </> location </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR.tid photographer) ++ "." ++ (pad index) ++ (takeExtension filename)
        where
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = case session of
                    School -> "8"
                    _ -> "9"
            shootingId = case shooting of
                    Normal -> "1"
                    ReShoot -> "2"
            pad x = strPadLeft '0' 3 (show x)
                



mkDagsdatoPath :: Dagsdato -> Photographee -> String -> String -> UTCTime -> FilePath
mkDagsdatoPath NoDagsdato _ _ _ _ = throw ConfigDagsdatoMissing
mkDagsdatoPath (Dagsdato dagsdatoDir) photographee location filename time = dagsdatoDir </>  ( date ++ " - " ++ location )</> grade </> (name ++ " - " ++ tea) </> filename
        where
            tea = _tea photographee
            name = _name photographee
            grade = _grade photographee 
            date = getDate time


actions :: ShakeConfig -> Photographee -> String -> UTCTime -> Rules ()
actions config photographee location time = do
        -- badIO
        dagsdato <- liftIO $ getDagsdato config
        dumpFiles <- liftIO $ getDumpFiles config
        doneshooting <- liftIO $ getDoneshooting config
        photographer <- liftIO $ getPhotographer config
        session <- liftIO $ getSession config
        shooting <- liftIO $ getShooting config
        -- badIO

        -- sortDumpFiles
        -- zip with 1 2 3 ..
        -- pad with 000

        ifor_ (sort dumpFiles) $ \ index dumpFile -> do
            let doneshootingFile = mkDoneshootingPath doneshooting photographee location photographer session shooting (takeFileName dumpFile) index

            let dagsdatoFile = mkDagsdatoPath dagsdato photographee location (takeFileName dumpFile) time

            want [doneshootingFile, dagsdatoFile] 

            doneshootingFile %> \f -> do
                copyFile' dumpFile f

            dagsdatoFile %> \f -> do
                copyFile' dumpFile f
