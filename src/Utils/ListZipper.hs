{-# LANGUAGE TemplateHaskell #-}
module Utils.ListZipper
    ( ListZipper(..)
    , focus
    , rights
    , lefts
    , mapFocus
    ) where


import Utils.Comonad
import Data.Aeson.TH (deriveJSON, defaultOptions)

data ListZipper a = ListZipper [a] a [a] deriving (Show) 

deriveJSON defaultOptions ''ListZipper


back :: ListZipper a -> ListZipper a
back (ListZipper (l:ls) a rs) = ListZipper ls l (a:rs)
back (ListZipper [] a rs) = ListZipper [] a rs


forward :: ListZipper a -> ListZipper a
forward (ListZipper ls a (r:rs)) = ListZipper (a:ls) r rs
forward (ListZipper ls a []) = ListZipper ls a []


lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls


rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs


focus :: ListZipper a -> a
focus zipper = extract zipper


mapFocus :: (a -> a) -> ListZipper a -> ListZipper a
mapFocus f (ListZipper ls a xs) = ListZipper ls (f a) xs


instance Functor ListZipper where
    fmap f (ListZipper ls a rs) = ListZipper (fmap f ls) (f a) (fmap f rs)


instance Comonad ListZipper where
    extract (ListZipper _ a _) = a
    duplicate z = ListZipper lefts' z rights'
        where 
            lefts' = reverse (iterate back z)
            rights' = iterate forward z

