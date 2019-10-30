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
    , grades
    , yesGrades
    , noGrades
    , toMaybe
    , delete 
    ) where

import Data.Maybe
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

import Prelude (($))


type Grade = String

data GradesF a
    = YesGrades (ListZipper a)
    | NoGrades 
    deriving (Show, Eq, Generic, ToJSON, FromJSON, Functor, Foldable, Traversable)


type Grades = GradesF Grade


toMaybe :: GradesF a -> Maybe (ListZipper a)
toMaybe = grades Nothing Just


yesGrades :: ListZipper Grade -> Grades
yesGrades = YesGrades


noGrades :: Grades
noGrades = NoGrades


grades :: a -> (ListZipper b -> a) -> GradesF b -> a
grades f g = \case
    NoGrades -> f
    YesGrades x -> g x


getGrades :: FP -> TerminalM Grades Grades
getGrades = readFile 


setGrades :: FP -> Grades -> TerminalM Grades ()
setGrades = writeFile 


delete :: Grades -> Grades
delete = grades noGrades $ \zipper ->
    case zipper of
        ListZipper [] a [] -> noGrades
        ListZipper [] a (r:rs) -> yesGrades $ ListZipper [] r rs
        ListZipper (l:ls) a rs -> yesGrades $ ListZipper ls l rs 
