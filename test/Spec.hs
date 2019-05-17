import Development.Shake.FilePath

import Test.Tasty
import Test.Tasty.Golden

import System.Directory

import qualified Data.ByteString.Lazy as LBS

import PhotoShake
import PhotoShake.ShakeConfig
import PhotoShake.Photographee


main :: IO ()
main = do
    golden <- goldenTests
    defaultMain $ testGroup "tests" [ golden ]


goldenTests :: IO TestTree
goldenTests = do
    config <- toShakeConfig "test/config.cfg"    
    let doneshootingDir = _doneshootingDir config
    let dagsdatoDir = _dagsdatoDir config
    
    let location = _location config
    let photographeeId = "5678"
    photographee <- findPhotographee location photographeeId 

    let ident = _ident photographee
    let goldenDir = "test" </> ident 

    createDirectoryIfMissing False doneshootingDir
    removeDirectoryRecursive doneshootingDir 

    createDirectoryIfMissing False dagsdatoDir
    removeDirectoryRecursive dagsdatoDir 

    myShake config photographee

    let doneshootingPath = doneshootingDir </> mkDoneshootingPath photographee
    let dagsdatoPath = dagsdatoDir </> mkDagsdatoPath photographee

    doneShootingFiles <- listDirectory doneshootingPath
    dagsdatoFiles <- listDirectory dagsdatoPath

    -- overvej refac
    return $ testGroup "all files moved" $ 
        [ goldenVsString
            (takeBaseName file)
            goldenFile
            (LBS.readFile file)
        | file <- fmap (\x -> doneshootingPath </> x) doneShootingFiles --could be nicer
        , let goldenFile = replaceDirectory file goldenDir
        ] ++    
        [ goldenVsString
            (takeBaseName file)
            goldenFile
            (LBS.readFile file)
        | file <- fmap (\x -> dagsdatoPath </> x) dagsdatoFiles --could be nicer
        , let goldenFile = replaceDirectory file goldenDir
        ]    
