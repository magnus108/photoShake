{-# LANGUAGE OverloadedStrings #-}
module PhotoShake
    ( entry
    , myShake
    , opts
    , mkPhotographeePath
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


mkPhotographeePath :: Photographee -> FilePath
mkPhotographeePath photographee = grade </> ident
        where
            ident = _ident photographee
            grade = _grade photographee


actions :: ShakeConfig -> Photographee -> Rules ()
actions config photographee = do
        let outDir = _outDir config
        let dumpFiles = _dumpFiles config

        forM_ dumpFiles $ \ dumpFile -> do
            let outFile = outDir </> mkPhotographeePath photographee </> takeFileName dumpFile
            want [outFile] 
            outFile %> \f -> do
                copyFile' dumpFile f
