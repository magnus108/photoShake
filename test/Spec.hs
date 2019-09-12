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
    dagsdatoX <- getDagsdato config
    doneshootingX <- getDoneshooting config
    
    location <- getLocationFile config
    --
    --- ??????
    case location of
        NoLocation -> error "no location in test eeee"
        Location xxx -> do

            let photographeeId = "5678"
            photographee <- findPhotographee xxx photographeeId 

            let ident = _ident photographee
            let goldenDir = "test" </> ident 

            -- uglys
            doneshooting (return ()) (\f -> do
                    createDirectoryIfMissing False f
                    removeDirectoryRecursive f) doneshootingX

            dagsdato (return ()) (\f -> do
                    createDirectoryIfMissing False f
                    removeDirectoryRecursive f) dagsdatoX


            let day = fromGregorian 2009 12 31
            let time = UTCTime day (secondsToDiffTime 0)

            myShake config photographee (takeBaseName xxx) time False

            -- bads 
            photographer <- getPhotographer config
            session <- getSession config
            shooting <- getShooting config


            -- de lader til at være en fejl at disse paths ligger her. og at null og 0 er med
            -- can throw error fixxxx
            let doneshootingPath = takeDirectory $ mkDoneshootingPath doneshootingX photographee (takeBaseName xxx) photographer session shooting "null" 0
            let dagsdatoPath = takeDirectory $ mkDagsdatoPath dagsdatoX photographee (takeBaseName xxx) "null" time

            doneShootingFiles <- listDirectory doneshootingPath
            dagsdatoFiles <- listDirectory dagsdatoPath

            -- overvej refac
            -- der er fejl i og med extension ikke er med i output
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
