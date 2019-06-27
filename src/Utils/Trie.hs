{-# LANGUAGE DeriveFunctor #-}
module Utils.Trie
    ( Trie(..)
    , TrieZipperF(..)
    , empty
    , insert
    , fromTrie
    , fromTrieF
    , Mu(..)
--    , MyTrie
    , ContextF(..)
    , TrieZipper(..)
    , fromTrieZipper
    , fromTrieZipperF
--    , complete
--    , tryTo
 --   , up
    ) where


import Data.Map (Map)
import qualified Data.Map as Map

import Utils.Comonad 


newtype Mu f = InF { outF :: f (Mu f) } 

data Stream a = Cons a (Stream a) deriving (Eq, Ord, Show, Functor)

hd :: Stream a -> a
hd (Cons x _ ) = x

data Trie a = Trie (Map a (Trie a)) Bool deriving (Show)
data TrieF k v = TrieF (Map k v) Bool deriving (Functor, Show)


data ContextF k v = ContextF ((k, Bool), Map k (TrieF k v)) deriving (Functor, Show)
data Context a = Context ((a, Bool), Map a (Trie a)) deriving (Show)


data TrieZipperF k v = TrieZipperF (TrieF k v, Stream (ContextF k v)) deriving (Functor, Show)
data TrieZipper a = TrieZipper (Trie a, Stream (Context a)) deriving (Show)


instance Comonad (TrieZipperF k) where
    extract xs = case fromTrieZipperF xs of
            (TrieZipper (t, Cons (Context ((a,_),_)) ctx)) -> t
    duplicate a = undefined
    -- duplicate a = ListZipper (shift back') a (shift forward')
      --  where shift move = tail $ iterate' move a


type MyTrieZipper a = Mu (TrieZipperF a)


fromTrieF :: TrieF a (Trie a) -> Trie a
fromTrieF (TrieF xs b) = Trie xs b

fromTrie :: Trie a -> TrieF a (Trie a)
fromTrie (Trie xs b) = TrieF xs b

fromContextF :: ContextF a (Trie a) -> Context a
fromContextF (ContextF (x, xs)) = Context (x, fmap fromTrieF xs)

fromContext :: Context a -> ContextF a (Trie a)
fromContext (Context (x, xs)) = ContextF (x, fmap fromTrie xs)

fromTrieZipper :: TrieZipper a -> TrieZipperF a (Trie a)
fromTrieZipper (TrieZipper (x,xs)) = TrieZipperF $ (fromTrie x, fmap fromContext xs)

fromTrieZipperF :: TrieZipperF a (Trie a) -> TrieZipper a
fromTrieZipperF (TrieZipperF (x,xs)) = TrieZipper $ (fromTrieF x, fmap fromContextF xs)


empty :: Ord a => Trie a
empty = Trie Map.empty False


insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie tries _) = Trie tries True
insert word@(firstChar : rest) (Trie tries wordEnd) =
    case Map.lookup firstChar tries of
        Nothing ->
            insert word (Trie (Map.insert firstChar empty tries) wordEnd)
        Just trie ->
            Trie (Map.insert firstChar (insert rest trie) tries) wordEnd


up :: Ord a => TrieZipper a -> TrieZipper a  
up (TrieZipper (Trie item b', Cons (Context ((x, b), xs)) tl)) =
    TrieZipper (Trie (Map.singleton x (Trie (Map.union item xs) b')) b,  tl)
--up root = root 



infixr 5 <:>
(<:>) :: a -> Stream a -> Stream a
(<:>) = Cons


tryTo :: Ord a => [a] -> TrieZipper a -> TrieZipper a 
tryTo [] xs = xs
tryTo (firstChar : rest) this@(TrieZipper (Trie tries b, bs)) =
    case Map.lookup firstChar tries of
        Nothing -> this
        Just r -> 
            tryTo rest (TrieZipper (r, (Context ((firstChar, b), Map.delete firstChar tries))<:>bs))

{-
complete :: Ord a => [a] -> Trie a -> [[a]]
complete [] (Trie tries wordEnd) =
    (if wordEnd then [[]] else []) ++
    concat [map (char :) (complete [] trie) | (char, trie) <- Map.toList tries]
complete (firstChar : rest) (Trie tries _) =
    maybe [] (map (firstChar :) . complete rest) (Map.lookup firstChar tries)

prefix :: Ord a => Trie a -> [[a]]
-}
