{-# LANGUAGE OverloadedStrings #-}
module PhotoShake
    ( entry
    , myShake
    , opts
    , mkDoneshootingPath
    , mkDagsdatoPath
    ) where

import Development.Shake
import Development.Shake.FilePath

import PhotoShake.ShakeConfig
import PhotoShake.Photographee

import Control.Monad


entry :: IO ()
entry = do
    config <- toShakeConfig "config.cfg"
    let location = _location config
    photographeeId <- getLine
    photographee <- findPhotographee location photographeeId 
    myShake config photographee


shakeDir :: FilePath
shakeDir = "._build"


opts :: ShakeOptions
opts = shakeOptions { shakeFiles = shakeDir
                    , shakeProgress = progressSimple -- should change
                    , shakeThreads = 0
                    , shakeColor = True
                    }


myShake :: ShakeConfig -> Photographee -> IO ()
myShake config photographee = shake opts $ actions config photographee


mkDoneshootingPath :: Photographee -> FilePath
mkDoneshootingPath photographee = grade </> ident
        where
            ident = _ident photographee
            grade = _grade photographee


mkDagsdatoPath:: Photographee -> FilePath
mkDagsdatoPath photographee = grade </> ident
        where
            ident = _ident photographee
            grade = _grade photographee
            

actions :: ShakeConfig -> Photographee -> Rules ()
actions config photographee = do
        let doneshootingDir = _doneshootingDir config
        let dagsdatoDir = _dagsdatoDir config
        let dumpFiles = _dumpFiles config

        forM_ dumpFiles $ \ dumpFile -> do
            let doneshootingFile = doneshootingDir </> mkDoneshootingPath photographee </> takeFileName dumpFile
            let dagsdatoFile = dagsdatoDir </> mkDagsdatoPath photographee </> takeFileName dumpFile
            want [doneshootingFile, dagsdatoFile] 

            doneshootingFile %> \f -> do
                copyFile' dumpFile f

            dagsdatoFile %> \f -> do
                copyFile' dumpFile f
