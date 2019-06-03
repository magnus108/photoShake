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

import PhotoShake.Dagsdato
import PhotoShake.Doneshooting
import PhotoShake.Shooting
import PhotoShake.Session
import qualified PhotoShake.Photographer as PR

import Control.Monad



entry :: IO ()
entry = do
    config <- toShakeConfig "config.cfg"
    let locationConfig = _locationConfig config
    locationFile <- getLocationFile locationConfig

    photographeeId <- getLine
    photographee <- findPhotographee locationFile photographeeId 

    -- ehh
    let location = takeBaseName locationFile
    myShake config photographee location


shakeDir :: FilePath
shakeDir = "._build"


opts :: ShakeOptions
opts = shakeOptions { shakeFiles = shakeDir
                    , shakeProgress = progressSimple -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }


myShake :: ShakeConfig -> Photographee -> String -> IO ()
myShake config photographee location = shake opts $ actions config photographee location


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
                



mkDagsdatoPath :: FilePath -> Photographee -> String -> String -> FilePath
mkDagsdatoPath dagsdatoDir photographee location filename = dagsdatoDir </> location </> grade </> (name ++ " - " ++ tea) </> filename
        where
            tea = _tea photographee
            name = _name photographee
            grade = _grade photographee 


actions :: ShakeConfig -> Photographee -> String -> Rules ()
actions config photographee location = do
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

            let dagsdatoFile = mkDagsdatoPath (unDagsdato dagsdato) photographee location (takeFileName dumpFile)

            want [doneshootingFile, dagsdatoFile] 

            doneshootingFile %> \f -> do
                copyFile' dumpFile f

            dagsdatoFile %> \f -> do
                copyFile' dumpFile f
