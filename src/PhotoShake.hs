{-# LANGUAGE OverloadedStrings #-}
module PhotoShake
    ( entry
    , myShake
    , opts
    , mkDoneshootingPath
    , mkDagsdatoPath
    ) where

import Development.Shake hiding (Normal)
import Development.Shake.FilePath

import PhotoShake.ShakeConfig
import PhotoShake.Photographee

import PhotoShake.Location
import PhotoShake.Dagsdato
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session
import qualified PhotoShake.Photographer as PR

import Control.Monad

import Data.Time.Format
import Data.Time.Clock



getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d" 



entry :: IO ()
entry = do
    config <- toShakeConfig Nothing "config.cfg"
    location <- getLocationFile config

    photographeeId <- getLine
    photographee <- findPhotographee (unLocation location) photographeeId 

    ---ehhh2
    time <- getCurrentTime
    -- ehh
    -- can make error
    myShake config photographee (takeBaseName (unLocation location)) time


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


mkDoneshootingPath :: FilePath -> Photographee -> String -> PR.Photographer -> Session -> Shooting -> String -> FilePath
mkDoneshootingPath doneshootingDir photographee location photographer session shooting filename = doneshootingDir </> location </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR.tid photographer) ++ "." ++ filename
        where
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = case session of
                    School -> "8"
                    _ -> "9"
            shootingId = case shooting of
                    Normal -> "1"
                    ReShoot -> "2"
                



mkDagsdatoPath :: FilePath -> Photographee -> String -> String -> UTCTime -> FilePath
mkDagsdatoPath dagsdatoDir photographee location filename time = dagsdatoDir </>  ( date ++ " - " ++ location )</> grade </> (name ++ " - " ++ tea) </> filename
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

        forM_ dumpFiles $ \ dumpFile -> do
            let doneshootingFile = mkDoneshootingPath (unDoneshooting doneshooting) photographee location photographer session shooting (takeFileName dumpFile)

            let dagsdatoFile = mkDagsdatoPath (unDagsdato dagsdato) photographee location (takeFileName dumpFile) time

            want [doneshootingFile, dagsdatoFile] 

            doneshootingFile %> \f -> do
                copyFile' dumpFile f

            dagsdatoFile %> \f -> do
                copyFile' dumpFile f
