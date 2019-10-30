{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              

module PhotoShake.Control
    ( Results(..)
    , Errors(..)
    , Error(..)
    , controlXMP
    , empty
    ) where

import Prelude ((+))
import Data.Ord
import Data.Bool
import Data.String
import Data.Tuple
import Data.Maybe
import Data.Eq
import Data.List
import Data.List.Extra
import Text.Show
import System.IO 
import Data.Int
import System.FilePath
import Control.Applicative ((<*>), (<$>))
import Control.Monad
import Data.Function
import System.Directory


import Data.ByteString.UTF8 (toString)
import qualified Data.ByteString as BS
import qualified PhotoShake.Camera as Camera
import qualified PhotoShake.Photographee as Photographee
import qualified PhotoShake.Doneshooting as Doneshooting
import qualified PhotoShake.Grade as Grade
import qualified PhotoShake.Location as Location
import qualified PhotoShake.Rating as Rating
import Utils.ByteStringExtra (between)
import Utils.Comonad 

import qualified PhotoShake.ShakeConfig as Config


import Data.Int

substring :: String -> String -> Bool
substring (x:xs) [] = False
substring xs ys
    | prefix xs ys = True
    | substring xs (tail ys) = True
    | otherwise = False

prefix :: String -> String -> Bool
prefix [] ys = True
prefix (x:xs) [] = False
prefix (x:xs) (y:ys) = (x == y) && prefix xs ys

only1With5_ ::  FilePath -> IO Int
only1With5_ filepath = do
    content <- BS.readFile $ filepath  -<.> "xmp"
    if (substring "<xmp:Rating>5</xmp:Rating>" (toString content)) then
        return 1
    else
        return 0


atleast5With1_ :: FilePath -> IO Int
atleast5With1_ filepath = do
    content <- BS.readFile $ filepath  -<.> "xmp"
    if (substring "<xmp:Rating>1</xmp:Rating>" (toString content)) then
        return 1
    else
        return 0



data Error 
    = Exactly1With5
    | Atleast5With1
    deriving (Show,Eq)


exactly1With5 :: Error
exactly1With5 = Exactly1With5


atleast5With1 :: Error
atleast5With1 = Atleast5With1


--data Result = Errors Photographee.Photographee [Error]
data Errors = Errors String [Error]
    deriving (Show, Eq)


data Results = Results [Errors]
    deriving (Show, Eq)

empty :: Results
empty = Results []


parseRating :: FilePath -> IO Rating.Rating
parseRating fp = do
    content <- BS.readFile fp -- could be lazy
    let prefix = "<xmp:Rating>"
    let postfix = "</xmp:Rating>"
    let rating = between prefix postfix content
    return (Rating.fromString (toString rating))

what :: Grade.Grade -> FilePath -> FilePath -> IO Results  --bads
what doneshootingDir location grade = do
    let cameras = Camera.supported
    let extension = fmap Camera.toString cameras 
    let name = takeBaseName location --bads

    eh <- sequence $ fmap (\ext -> do
            let path = doneshootingDir </> name </> ext </> grade
            putStrLn $ "hsdhg"
            putStrLn $ path 
            files <- listDirectory path 
            putStrLn $ "hhg"
            putStrLn $ show files
            let filtered = groupOn (\f -> (splitOn "."  f) !! 1) $ filter (\f -> isExtensionOf ext f) (sort files)
            
            let studentAndCrs = fmap (\xx -> ((splitOn "." (xx !! 0)) !! 1 , xx)) filtered

            studentAndCrs' <- mapM (\xx -> do
                        res <- filterM (\f -> doesFileExist (path </> f -<.> "xmp")) (snd xx)
                        return (fst xx, res)
                        ) $ studentAndCrs

            gg <- mapM (\xxx -> do
                    let xxxx = fst xxx
                    only1with5' <- mapM (only1With5_) $ (fmap (\xxxx -> (path </> xxxx))) (snd xxx)
                    let sum = 1 == (foldl (\ss acc -> ss + acc) 0 (only1with5'))
                    atleast5With1' <- mapM (atleast5With1_) $ (fmap (\xxxx -> (path </> xxxx))) (snd xxx)
                    let sum2 = 5 <= (foldl (\ss acc -> ss + acc) 0 (atleast5With1')) 
                    return (xxxx, sum, sum2)
                ) studentAndCrs'

            putStrLn $ "hh"
            putStrLn $ show gg
 
            let yy = filter (\(xxxx, sum, sum2) -> not sum || not sum2 ) gg

            let abc = fmap (\(xxxx, sum, sum2) ->  if (sum && sum2) then
                            Errors xxxx [atleast5With1, exactly1With5]
                        else if sum then
                            Errors xxxx [atleast5With1]
                        else 
                            Errors xxxx [exactly1With5] 
                        ) yy
            putStrLn $ show abc
            
            return abc
            ) extension

   -- eh
    return (Results (join eh))


controlXMP :: Config.ShakeConfig -> IO Results -- ku også lave kontrol af klasse
controlXMP config = do
    doneshooting <- Config.getDoneshooting config
    locationFile <- Config.getLocationFile config
    grades <- Config.getGrades config
    fromMaybe (return (Results [])) $ 
            (what . extract) <$> (Grade.toMaybe grades) <*> (Location.toMaybe locationFile) <*> (Doneshooting.toMaybe doneshooting)
    



{-
controlXMP :: ShakeConfig -> Grade -> IO Result
controlXMP config grade = do 
    xxx <- getDoneshooting config
    doneshooting (return NoErrors) (\doneshootingDir -> do
            locationFile <- getLocationFile config
            location ( return NoErrors) (\ loc -> do
                    let path = doneshootingDir </> (takeBaseName loc) </> "cr2" </> grade
                    let path2 = doneshootingDir </> (takeBaseName loc) </> "cr3" </> grade
                    files <- try $ listDirectory path :: IO (Either SomeException [FilePath])
                    files2 <- try $ listDirectory path2 :: IO (Either SomeException [FilePath])
                    let files3 = liftA2 (++) files files2
                    putStrLn $ show files3
                    case files3 of
                        Left z -> return Empty
                        Right [] -> return Empty
                        Right z -> do
                            let what = groupOn (\f -> (splitOn "."  f) !! 1) $ filter (\f -> isExtensionOf "cr2" f || isExtensionOf "cr3" f) (sort z)

                            let cr2s = fmap (\xx -> ((splitOn "." (xx !! 0)) !! 1 , xx)) what  

                            cr2s' <- mapM (\xx -> do
                                        res <- filterM (\f -> do
                                             a <- doesFileExist (path </> f -<.> "xmp")
                                             b <- doesFileExist (path2 </> f -<.> "xmp")
                                             return (a || b)
                                             ) (snd xx)
                                        return (fst xx, res)
                                        ) $ cr2s

                            gg <- mapM (\xxx -> do
                                    let xxxx = fst xxx
                                    let lencheck = length (snd xxx) >= 6
                                    only1with5' <- mapM (only1With5) (fmap (\xxxx -> if isExtensionOf "cr2" xxxx then (path </> xxxx) else (path2 </> xxxx)) (snd xxx))
                                    let sum = 1 == (foldl (\ss acc -> ss + acc) 0 (only1with5')) 
                                    atleast5With1' <- mapM (atleast5With1) (fmap (\xxxx -> if isExtensionOf "cr2" xxxx then (path </> xxxx) else (path2 </> xxxx)) (snd xxx))
                                    let sum2 = 5 <= (foldl (\ss acc -> ss + acc) 0 (atleast5With1')) 
                                    return (xxxx, lencheck, sum, sum2)
                                ) cr2s'

                            let yy = filter (\(xxxx, lencheck, sum, sum2) -> not sum || not sum2 || not lencheck) gg
                            case yy of
                                [] -> return NoErrors
                                xx -> return $ Errors yy
                                ) locationFile ) xxx
                            -}
