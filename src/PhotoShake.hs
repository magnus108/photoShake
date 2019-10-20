{-# LANGUAGE OverloadedStrings #-}
module PhotoShake
    ( entry
    , myShake
    , opts
    , mkDoneshootingPath
    , mkDagsdatoPath
    ) where

import Prelude hiding (toInteger)
import Data.Strings

import Control.Concurrent.MVar

import qualified PhotoShake.Id as Id

import Development.Shake hiding (Normal)
import Development.Shake.FilePath

import qualified PhotoShake.Camera as Camera
import PhotoShake.ShakeConfig
import PhotoShake.Dump
import PhotoShake.Photographee

import PhotoShake.Location
import PhotoShake.Dagsdato
import PhotoShake.Doneshooting hiding (getDoneshooting)
import PhotoShake.Shooting hiding (getShootings)
import qualified PhotoShake.Session as Session
import PhotoShake.ShakeError

import qualified PhotoShake.Photographer as PR

import Utils.ListZipper

import Data.Time.Format
import Data.Time.Clock

import Data.List
import Data.List.Index



getDate :: UTCTime -> String
getDate = formatTime defaultTimeLocale "%Y - %m%d" 



entry :: IO ()
entry = do
    config <- toShakeConfig Nothing "config.cfg"
    xxxx <- getLocationFile config
    -- ehh
    -- can make error
    photographeeId <- getLine
    photographee <- findPhotographee xxxx (Id.fromString photographeeId )

    ---ehhh2
    time <- getCurrentTime
    location (error "no location given") (\xxx -> do
        maybe (error "no photographee found") (\p -> 
            myShake config p (takeBaseName xxx) time False) photographee) xxxx


shakeDir :: FilePath
shakeDir = "._build"


opts :: Photographee -> ShakeConfig -> ShakeOptions
opts photographee config = shakeOptions { shakeFiles = shakeDir
                    , shakeProgress = progress -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }
    where
        progress p = do
            lock <- newMVar ()
            progressDisplay 0.05 (\s -> do
                a <- takeMVar lock
                setBuild config s photographee
                putMVar lock a
                ) p


myShake :: ShakeConfig -> Photographee -> String -> UTCTime -> Bool -> IO ()
myShake config photographee location time removeIt = do
    shake (opts photographee config) $ actions config photographee location time removeIt


mkDoneshootingPath :: Doneshooting -> Photographee -> String -> PR.Photographer -> Session.Session -> Shooting -> String -> Int -> Camera.Camera -> FilePath
mkDoneshootingPath xxx photographee location photographer session shooting filename index camera = 
    doneshooting (throw ConfigDoneshootingMissing) (\doneshootingDir -> doneshootingDir </> location 
            </> extension </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR._tid photographer) ++ "." ++ (pad $ index + 1) ++ (takeExtension filename)) xxx
        where
            extension =  Camera.camera "cr2" "cr3" camera
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = show $ Session.toInteger session 
            shootingId = Session.session  --- this wrongs
                    (Session.type_ (show $ toInteger shooting) ("3"))
                    (show $ toInteger shooting) session
            pad x = strPadLeft '0' 3 (show x)
                

mkDoneshootingPathJpg :: Doneshooting -> Photographee -> String -> PR.Photographer -> Session.Session -> Shooting -> String -> Int -> Camera.Camera -> FilePath
mkDoneshootingPathJpg xxx photographee location photographer session shooting filename index camera = 
    doneshooting (throw ConfigDoneshootingMissing) (\doneshootingDir -> doneshootingDir </> location </> extension
            </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR._tid photographer) ++ "." ++ (pad $ index + 1) ++ (takeExtension filename)) xxx
        where
            extension =  Camera.camera "cr2" "cr3" camera
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = show $ Session.toInteger session 
            shootingId = Session.session  --- this wrongs
                    (Session.type_ (show $ toInteger shooting) ("3"))
                    (show $ toInteger shooting) session
            pad x = strPadLeft '0' 3 (show x)


mkDagsdatoPath :: Dagsdato -> Photographee -> String -> String -> UTCTime -> FilePath
mkDagsdatoPath xxx photographee location filename time =
    dagsdato (throw ConfigDagsdatoMissing) (\dagsdatoDir -> dagsdatoDir </>  ( date ++ " - " ++ location )</> grade </> (name ++ " - " ++ tea) </> filename) xxx
        where
            tea = _tea photographee
            name = _name photographee
            grade = _grade photographee 
            date = getDate time


actions :: ShakeConfig -> Photographee -> String -> UTCTime -> Bool -> Rules ()
actions config photographee location time removeIt = do
        --
        -- badIO
        dagsdato <- liftIO $ getDagsdato config
        cameras <- liftIO $ getCameras config

        dump_ <- liftIO $ getDump config 
        (DumpFiles dumpFiles) <- liftIO $ getDumpFiles dump_ cameras

        dagsdatoBackup <- liftIO $ getDagsdatoBackup config
        doneshootingBackup <- liftIO $ getDoneshootingBackup config
        
        doneshooting <- liftIO $ getDoneshooting config
        photographer <- liftIO $ getPhotographer config
        session <- liftIO $ getSession config
        shooting <- liftIO $ getShooting config
        -- badIO

        ifor_ (sort dumpFiles) $ \ index (cr, jpg) -> do
            let extensionFIXME = Camera.cameras (error "fuck") (\c -> Camera.camera "cr2" "cr3" (focus c)) cameras
            let doneshootingCr = Camera.cameras (error "FUCK") (\c -> mkDoneshootingPath doneshooting photographee location photographer session shooting (takeFileName cr) index (focus c) -<.> extensionFIXME) cameras
            let doneshootingJpg = Camera.cameras (error "fuck") (\c -> mkDoneshootingPathJpg doneshooting photographee location photographer session shooting (takeFileName jpg) index (focus c) -<.> "jpg") cameras 


            --let doneshootingBackupCr2 = mkDoneshootingPath doneshootingBackup photographee location photographer session shooting (takeFileName cr2) index -<.> "cr2"
            --let doneshootingBackupJpg = mkDoneshootingPathJpg doneshootingBackup photographee location photographer session shooting (takeFileName jpg) index -<.> "jpg"

            let dagsdatoCr = mkDagsdatoPath dagsdato photographee location (takeFileName cr) time -<.> extensionFIXME
            let dagsdatoJpg = mkDagsdatoPath dagsdato photographee location (takeFileName jpg) time -<.> "jpg"

            let dagsdatoBackupCr = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName cr) time -<.> extensionFIXME
            let dagsdatoBackupJpg = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName jpg) time -<.> "jpg"

            want [doneshootingCr, doneshootingJpg, dagsdatoCr, dagsdatoJpg
                -- , doneshootingBackupCr2, doneshootingBackupJpg, 
                 , dagsdatoBackupCr, dagsdatoBackupJpg] 

            doneshootingCr %> \f -> do
                copyFile' cr f

            doneshootingJpg %> \f -> do
                copyFile' jpg f

            {-
            doneshootingBackupCr2 %> \f -> do
                copyFile' cr2 f

            doneshootingBackupJpg %> \f -> do
                copyFile' jpg f
            -}

            dagsdatoCr %> \f -> do
                copyFile' cr f

            dagsdatoJpg %> \f -> do
                copyFile' jpg f

            dagsdatoBackupCr %> \f -> do
                copyFile' cr f

            dagsdatoBackupJpg %> \f -> do
                copyFile' jpg f


        x <- liftIO $ getDump config
        dump (action $ return ()) (\fp -> do
                    liftIO $ setId config Id.noId
                    if removeIt then
                        action $ removeFilesAfter fp ["//*.CR2", "//*.JPG", "//*.cr2", "//*.jpg","//*.CR3","//*.cr3"]
                    else
                        return () ) x
