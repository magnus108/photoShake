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
import PhotoShake.Dump
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
            myShake config photographee (takeBaseName xxx) time False


shakeDir :: FilePath
shakeDir = "._build"


opts :: Photographee -> ShakeConfig -> ShakeOptions
opts photographee config = shakeOptions { shakeFiles = shakeDir
                    --, shakeProgress = progress -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }
  --where
   -- progress p = do
    --  program <- progressProgram
     -- progressDisplay 0.5 (\s -> do
       --     setBuilt config s photographee
        --    program s
         --   ) p


myShake :: ShakeConfig -> Photographee -> String -> UTCTime -> Bool -> IO ()
myShake config photographee location time removeIt = do
    shake (opts photographee config) $ actions config photographee location time removeIt


mkDoneshootingPath :: Doneshooting -> Photographee -> String -> PR.Photographer -> Session -> Shooting -> String -> Int -> FilePath
mkDoneshootingPath NoDoneshooting _ _ _ _ _ _ _ = throw ConfigDoneshootingMissing
mkDoneshootingPath (Doneshooting doneshootingDir) photographee location photographer session shooting filename index = doneshootingDir </> location </> "cr2" </> grade </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR.tid photographer) ++ "." ++ (pad $ index + 1) ++ (takeExtension filename)
        where
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = case session of
                    School -> "9"
                    _ -> "10"
            shootingId = 
                case session of
                    Kindergarten Group -> "3"
                    _ -> case shooting of
                            Normal -> "1"
                            ReShoot -> "2"
            pad x = strPadLeft '0' 3 (show x)
                

mkDoneshootingPathJpg :: Doneshooting -> Photographee -> String -> PR.Photographer -> Session -> Shooting -> String -> Int -> FilePath
mkDoneshootingPathJpg NoDoneshooting _ _ _ _ _ _ _ = throw ConfigDoneshootingMissing
mkDoneshootingPathJpg (Doneshooting doneshootingDir) photographee location photographer session shooting filename index = doneshootingDir </> location </> "cr2" </> "_webshop" </> sessionId ++ "." ++ tea ++ "." ++ shootingId ++ "." ++ (PR.tid photographer) ++ "." ++ (pad $ index + 1) ++ (takeExtension filename)
        where
            tea = _tea photographee
            grade = _grade photographee 
            sessionId = case session of
                    School -> "9"
                    _ -> "10"
            shootingId = 
                case session of
                    Kindergarten Group -> "3"
                    _ -> case shooting of
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


actions :: ShakeConfig -> Photographee -> String -> UTCTime -> Bool -> Rules ()
actions config photographee location time removeIt = do
        -- badIO
        dagsdato <- liftIO $ getDagsdato config
        dumpFiles <- liftIO $ getDumpFiles config

        dagsdatoBackup <- liftIO $ getDagsdatoBackup config
        doneshootingBackup <- liftIO $ getDoneshootingBackup config
        
        doneshooting <- liftIO $ getDoneshooting config
        photographer <- liftIO $ getPhotographer config
        session <- liftIO $ getSession config
        shooting <- liftIO $ getShooting config
        -- badIO

        ifor_ (sort dumpFiles) $ \ index (cr2, jpg) -> do
            let doneshootingCr2 = mkDoneshootingPath doneshooting photographee location photographer session shooting (takeFileName cr2) index -<.> "cr2"
            let doneshootingJpg = mkDoneshootingPathJpg doneshooting photographee location photographer session shooting (takeFileName jpg) index -<.> "jpg"


            --let doneshootingBackupCr2 = mkDoneshootingPath doneshootingBackup photographee location photographer session shooting (takeFileName cr2) index -<.> "cr2"
            --let doneshootingBackupJpg = mkDoneshootingPathJpg doneshootingBackup photographee location photographer session shooting (takeFileName jpg) index -<.> "jpg"

            let dagsdatoCr2 = mkDagsdatoPath dagsdato photographee location (takeFileName cr2) time -<.> "cr2"
            let dagsdatoJpg = mkDagsdatoPath dagsdato photographee location (takeFileName jpg) time -<.> "jpg"

            let dagsdatoBackupCr2 = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName cr2) time -<.> "cr2"
            let dagsdatoBackupJpg = mkDagsdatoPath dagsdatoBackup photographee location (takeFileName jpg) time -<.> "jpg"

            want [doneshootingCr2, doneshootingJpg, dagsdatoCr2, dagsdatoJpg
                -- , doneshootingBackupCr2, doneshootingBackupJpg, 
                 , dagsdatoBackupCr2, dagsdatoBackupJpg] 

            doneshootingCr2 %> \f -> do
                copyFile' cr2 f

            doneshootingJpg %> \f -> do
                copyFile' jpg f

            {-
            doneshootingBackupCr2 %> \f -> do
                copyFile' cr2 f

            doneshootingBackupJpg %> \f -> do
                copyFile' jpg f
            -}

            dagsdatoCr2 %> \f -> do
                copyFile' cr2 f

            dagsdatoJpg %> \f -> do
                copyFile' jpg f

            dagsdatoBackupCr2 %> \f -> do
                copyFile' cr2 f

            dagsdatoBackupJpg %> \f -> do
                copyFile' jpg f


        dump <- liftIO $ getDump config

        case dump of
            NoDump -> action $ return ()            
            Dump x -> do
                liftIO $ setIdSelection config (Idd "")
                if removeIt then
                    action $ removeFilesAfter x ["//*.CR2", "//*.JPG", "//*.cr2", "//*.jpg"]
                else
                    return () 
