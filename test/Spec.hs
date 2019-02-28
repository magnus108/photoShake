import Development.Shake.FilePath

import Test.Tasty
import Test.Tasty.Golden

import Development.Shake

import qualified Data.ByteString.Lazy as LBS

import PhotoShake
import PhotoShake.ShakeConfig
import Photographee


main :: IO ()
main = do
    golden <- goldenTests
    defaultMain $ testGroup "tests" [ golden ]


goldenTests :: IO TestTree
goldenTests = do
    config <- toShakeConfig "test/config.cfg"    
    let outDir = _outDir config
    
    let photographee = Photographee { _ident = "5678"
                                    , _grade = "9.A"
                                    , _name = "Christian Morling"
                                    }

    let goldenDir = "test" </> (_ident photographee)

    removeFiles outDir ["//*"]

    myShake config photographee

    files <- getDirectoryFilesIO outDir ["//*"]

    return $ testGroup "all files moved"
        [ goldenVsString
            (takeBaseName file)
            goldenFile
            (LBS.readFile file)
        | file <- fmap (\x -> outDir </> x) files --could be nicer
        , let goldenFile = replaceDirectory file goldenDir
        ]    

