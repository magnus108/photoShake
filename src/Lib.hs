module Lib
    ( someFunc
    ) where

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Control.Monad


dumpDir :: FilePath
dumpDir = "dump"

outDir :: FilePath
outDir = "out"


dumpFiles :: Action [FilePath]
dumpFiles = getDirectoryContents dumpDir


outFile :: FilePattern
outFile = outDir </> "*.pdf"

--------------------------------------------------------------------------------

shakeDir :: FilePath
shakeDir = "_build"

opts :: ShakeOptions
opts = shakeOptions { shakeFiles = shakeDir }


someFunc :: IO ()
someFunc = shakeArgs opts $ do    
    action $ do
        files <- dumpFiles
        forM_ files $ \ f ->  do
            let outFile = outDir </> takeBaseName f <.> ".pdf"
            need [ outFile ]

    outFile %> \f -> do 
        let dumpFile = dumpDir </> takeBaseName f <.> ".md"
        need [ dumpFile ]
        cmd "pandoc" [ dumpFile, "-o", f ]

    "clean" ~> removeFilesAfter shakeDir ["//*"]
