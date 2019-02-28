{-# LANGUAGE OverloadedStrings #-}
module PhotoShake
    ( entry
    , myShake
    , opts
    ) where

import Development.Shake
import Development.Shake.FilePath

import PhotoShake.ShakeConfig
import Photographee

import Control.Monad


entry :: IO ()
entry = do
    config <- toShakeConfig "config.cfg"
    let location = _location config
    photographeeId <- getLine
    photographee <- findPhotographee location photographeeId 
    myShake config photographee


shakeDir :: FilePath
shakeDir = "_build"


opts :: ShakeOptions
opts = shakeOptions { shakeFiles = shakeDir
                    , shakeProgress = progressSimple
                    }


actions :: ShakeConfig -> Photographee -> Rules ()
actions config photographee = do
        let outDir = _outDir config
        let dumpFiles = _dumpFiles config
        let photographeeId = _ident photographee
        forM_ dumpFiles $ \ dumpFile -> do
            let outFile = outDir </> photographeeId </> dumpFile 
            want [outFile] 
            outFile %> \f -> do
                copyFile' dumpFile f


myShake :: ShakeConfig -> Photographee -> IO ()
myShake config photographee = shake opts $ actions config photographee
