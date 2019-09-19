{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Utils.ListZipper
    ( ListZipper(..)
    , focus
    , rights
    , lefts
    , mapFocus
    , back
    , forward
    , toList
    , toList'
    , iextend
    ) where

import GHC.Generics
import Data.Aeson

import Data.Maybe

import Utils.Comonad

data ListZipper a = ListZipper [a] a [a] 
    deriving (Show, Eq, Generic, ToJSON, FromJSON)


back :: ListZipper a -> ListZipper a
back a = fromMaybe a (back' a)

back' :: ListZipper a -> Maybe (ListZipper a)
back' (ListZipper (l:ls) a rs) = Just (ListZipper ls l (a:rs))
back' (ListZipper [] _ _) = Nothing


forward :: ListZipper a -> ListZipper a
forward a = fromMaybe a (forward' a)


forward' :: ListZipper a -> Maybe (ListZipper a)
forward' (ListZipper ls a (r:rs)) = Just (ListZipper (a:ls) r rs)
forward' (ListZipper _ _ []) = Nothing


lefts :: ListZipper a -> [a]
lefts (ListZipper ls _ _) = reverse ls


rights :: ListZipper a -> [a]
rights (ListZipper _ _ rs) = rs


focus :: ListZipper a -> a
focus zipper = extract zipper


mapFocus :: (a -> a) -> ListZipper a -> ListZipper a
mapFocus f (ListZipper ls a xs) = ListZipper ls (f a) xs


iterate' :: ( a -> Maybe a) -> a -> [a]
iterate' f x = 
    case f x of
            Just x' -> x : (iterate' f x')
            Nothing -> [x]


toList :: ListZipper a -> [a]
toList (ListZipper ls x rs) = (reverse ls) ++ (x : rs)


toList' :: ListZipper a -> [a]
toList' (ListZipper ls x rs) = ls ++ (x : rs)


iextend :: (Int -> ListZipper a -> b) -> ListZipper a -> ListZipper b 
iextend f = fmap (\xs@(ListZipper ls _ _) -> f (length ls) xs) . duplicate


instance Functor ListZipper where
    fmap f (ListZipper ls a rs) = ListZipper (fmap f ls) (f a) (fmap f rs)

instance Comonad ListZipper where
    extract (ListZipper _ a _) = a
    duplicate a = ListZipper (shift back') a (shift forward')
        where shift move = tail $ iterate' move a

instance Foldable ListZipper where
  foldMap f (ListZipper l x r) =
    foldMap f l `mappend` f x `mappend` foldMap f r

instance Traversable ListZipper where
  traverse f (ListZipper l x r) =
    ListZipper <$> traverse f l <*> f x <*> traverse f r
