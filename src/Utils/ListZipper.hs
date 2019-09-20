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
    , iextend
    , sorted
    , insert
    ) where


import GHC.Generics
import Data.Aeson 
import Data.Maybe

import Utils.Comonad

data ListZipper a = ListZipper [a] a [a] 
    deriving (Show, Eq, Generic, ToJSON, FromJSON)

insert :: ListZipper a -> a -> ListZipper a
insert (ListZipper ls a rs) b = ListZipper (a:ls) b rs


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


iextend :: (Int -> ListZipper a -> b) -> ListZipper a -> ListZipper b 
iextend f = fmap (\xs@(ListZipper ls _ _) -> f (length ls) xs) . duplicate

--move me
insert' :: Ord a => a -> [a] -> [a]
insert' x [] = [x]
insert' x (y:ys) | x < y     = x:y:ys
                 | otherwise = y:(insert' x ys)

insert'' :: Ord a => a -> [a] -> [a]
insert'' x [] = [x]
insert'' x (y:ys) | x > y     = x:y:ys
                  | otherwise = y:(insert'' x ys)


sorted :: Ord a => [a] -> ListZipper a -> ListZipper a
sorted [] z = z
sorted (y:ys) (ListZipper ls a rs) | y < a = sorted ys $ ListZipper (insert'' y ls) a rs
                                   | otherwise = sorted ys $ ListZipper ls a (insert' y rs) 


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
