import Development.Shake.FilePath

import Test.Tasty
import Test.Tasty.Golden

import System.Directory

import qualified Data.ByteString.Lazy as LBS

import Data.Time

import PhotoShake
import PhotoShake.Location
import PhotoShake.ShakeConfig
import PhotoShake.Doneshooting
import PhotoShake.Dagsdato
import PhotoShake.Photographee


main :: IO ()
main = do
    golden <- goldenTests
    defaultMain $ testGroup "tests" [ golden ]


goldenTests :: IO TestTree
goldenTests = do
    config <- toShakeConfig Nothing "test/config.cfg"    
    
    --IO bads
    dagsdato <- getDagsdato config
    doneshooting <- getDoneshooting config
    
    location <- getLocationFile config

    let photographeeId = "5678"
    photographee <- findPhotographee (unLocation location) photographeeId 

    let ident = _ident photographee
    let goldenDir = "test" </> ident 

    createDirectoryIfMissing False (unDoneshooting doneshooting)
    removeDirectoryRecursive (unDoneshooting doneshooting)

    createDirectoryIfMissing False (unDagsdato dagsdato)
    removeDirectoryRecursive (unDagsdato dagsdato)


    let day = fromGregorian 2009 12 31
    let time = UTCTime day (secondsToDiffTime 0)

    myShake config photographee (takeBaseName (unLocation location)) time

    -- bads 
    photographer <- getPhotographer config
    session <- getSession config
    shooting <- getShooting config
    let doneshootingPath = takeDirectory $ mkDoneshootingPath (unDoneshooting doneshooting) photographee (takeBaseName (unLocation location)) photographer session shooting "null"
    let dagsdatoPath = takeDirectory $ mkDagsdatoPath (unDagsdato dagsdato) photographee (takeBaseName (unLocation location)) "null" time

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
