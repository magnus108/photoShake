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
    
    --IO bads
    let doneshootingConfig = _doneshootingConfig config
    let dagsdatoConfig = _dagsdatoConfig config
    dagsdatoDir <- getDagsdatoDir dagsdatoConfig
    doneshootingDir <- getDoneshootingDir doneshootingConfig
    
    let locationConfig = _locationConfig config
    locationFile <- getLocationFile locationConfig

    let photographeeId = "5678"
    photographee <- findPhotographee locationFile photographeeId 

    let ident = _ident photographee
    let goldenDir = "test" </> ident 

    createDirectoryIfMissing False doneshootingDir
    removeDirectoryRecursive doneshootingDir 

    createDirectoryIfMissing False dagsdatoDir
    removeDirectoryRecursive dagsdatoDir 

    let location = takeBaseName (locationFile)
    myShake config photographee location

    -- bads 
    photographer <- getPhotographer config
    session <- getSession config
    shooting <- getShooting config
    let doneshootingPath = takeDirectory $ mkDoneshootingPath doneshootingDir photographee location photographer session shooting "null"
    let dagsdatoPath = takeDirectory $ mkDagsdatoPath dagsdatoDir photographee location "null"

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
