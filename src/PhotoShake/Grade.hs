{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}                                                                                                                                                                              
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}                                                      

module PhotoShake.Grade
    ( Grade
    , Grades
    , GradeSelection
    , grades
    , yesGrades
    , noGrades
    , gradeSelection
    , yesGradeSelection
    , noGradeSelection
    ) where

import Data.String
import Data.Eq
import Text.Show
import GHC.Generics
import System.FilePath
import Data.Aeson
import Utils.Actions
import Utils.FP

import Utils.ListZipper

import Data.Functor
import Data.Traversable
import Data.Foldable


type Grade = String

data GradesF a
    = YesGrades a
    | NoGrades 
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)


type Grades = GradesF (ListZipper Grade)


data GradeSelection
    = YesGradeSelection Grade
    | NoGradeSelection
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

yesGradeSelection :: Grade -> GradeSelection
yesGradeSelection = YesGradeSelection

noGradeSelection :: GradeSelection
noGradeSelection = NoGradeSelection


gradeSelection :: a -> (Grade -> a) -> GradeSelection -> a
gradeSelection f g = \case
    NoGradeSelection -> f
    YesGradeSelection x -> g x

getGradeSelection :: FP -> TerminalM GradeSelection GradeSelection
getGradeSelection = readFile 

setGradesSelection :: FP -> GradeSelection -> TerminalM GradeSelection ()
setGradesSelection = writeFile 


yesGrades :: ListZipper Grade -> Grades
yesGrades = YesGrades


noGrades :: Grades
noGrades = NoGrades


grades :: a -> (ListZipper Grade -> a) -> Grades -> a
grades f g = \case
    NoGrades -> f
    YesGrades x -> g x


getGrades :: FP -> TerminalM Grades Grades
getGrades = readFile 


setGrades :: FP -> Grades -> TerminalM Grades ()
setGrades = writeFile 
