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
    let outDir = _outDir config
    
    let photographee = Photographee { _tea = "TEA_9882123"
                                    , _ident = "5678"
                                    , _grade = "9A"
                                    , _name = "Christian Morling"
                                    }

    let ident = _ident photographee
    let goldenDir = "test" </> ident 

    removeDirectoryRecursive outDir

    myShake config photographee

    let path = outDir </> mkPhotographeePath photographee

    files <- listDirectory path

    return $ testGroup "all files moved"
        [ goldenVsString
            (takeBaseName file)
            goldenFile
            (LBS.readFile file)
        | file <- fmap (\x -> path </> x) files --could be nicer
        , let goldenFile = replaceDirectory file goldenDir
        ]    

