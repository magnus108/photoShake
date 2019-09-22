{-# LANGUAGE TemplateHaskell #-}
module PhotoShake.Control
    ( controlXMP
    , Result(..)
    ) where

import PhotoShake.ShakeConfig
import PhotoShake.Location
import PhotoShake.Doneshooting hiding (getDoneshooting)
import PhotoShake.Photographee

import System.FilePath
import System.Directory

import Control.Exception

import Data.List

import Control.Applicative
import Control.Monad
import Data.List.Extra
import Prelude hiding (readFile)
import Data.ByteString.Lazy (readFile)
import Data.ByteString.Lazy.UTF8 (fromString, toString)

data Result = Errors [(String, Bool, Bool, Bool)]
            | NoErrors
            | Empty
    deriving (Show, Eq)


type Grade = String

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


only1With5 :: FilePath -> FilePath -> IO Integer
only1With5 root filepath = do
    content <- readFile $ root </> filepath  -<.> "xmp"
    if (substring "<xmp:Rating>5</xmp:Rating>" (toString content)) then
        return 1
    else
        return 0


atleast5With1 :: FilePath -> FilePath -> IO Integer
atleast5With1 root filepath = do
    content <- readFile $ root </> filepath  -<.> "xmp"
    if (substring "<xmp:Rating>1</xmp:Rating>" (toString content)) then
        return 1
    else
        return 0

controlXMP :: ShakeConfig -> Grade -> IO Result
controlXMP config grade = do 
    xxx <- getDoneshooting config
    doneshooting (return NoErrors) (\doneshootingDir -> do
            locationFile <- getLocationFile config
            location ( return NoErrors) (\ loc -> do
                    let path = doneshootingDir </> (takeBaseName loc) </> "cr2" </> grade
                    files <- try $ listDirectory path :: IO (Either SomeException [FilePath])
                    case files of
                        Left z -> return Empty
                        Right [] -> return Empty
                        Right z -> do
                            let what = groupOn (\f -> (splitOn "."  f) !! 1) $ filter (\f -> isExtensionOf "cr2" f) (sort z)

                            let cr2s = fmap (\xx -> ((splitOn "." (xx !! 0)) !! 1 , xx)) what  


                            cr2s' <- mapM (\xx -> do
                                        res <- filterM (\f -> do
                                             doesFileExist (path </> f -<.> "xmp")
                                             ) (snd xx)
                                        return (fst xx, res)
                                        ) $ cr2s
             
                            gg <- mapM (\xxx -> do
                                    let xxxx = fst xxx
                                    let lencheck = length (snd xxx) >= 6
                                    only1with5' <- mapM (only1With5 path) (snd xxx)
                                    let sum = 1 == (foldl (\ss acc -> ss + acc) 0 only1with5')
                                    atleast5With1' <- mapM (atleast5With1 path) (snd xxx)
                                    let sum2 = 5 <= (foldl (\ss acc -> ss + acc) 0 atleast5With1')
                                    return (xxxx, lencheck, sum, sum2)
                                ) cr2s'

                            let yy = filter (\(xxxx, lencheck, sum, sum2) -> not sum || not sum2 || not lencheck) gg
                            case yy of
                                [] -> return NoErrors
                                xx -> return $ Errors yy
                                ) locationFile ) xxx
