module Utils.Trie
    ( Trie(..)
    , TrieZipper(..)
    , empty
    , insert
    , complete
    , tryTo
    , up
    ) where


import Data.Map (Map)
import qualified Data.Map as Map


data Trie a = Trie (Map a (Trie a)) Bool deriving Show

data Context a = Context ((a, Bool), Map a (Trie a)) deriving Show

data TrieZipper a = TrieZipper (Trie a, [Context a] ) deriving Show

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
up (TrieZipper (Trie item b', Context ((x, b), xs):tl)) = TrieZipper (Trie (Map.singleton x (Trie (Map.union item xs) b')) b,  tl)
up root = root 


tryTo :: Ord a => [a] -> TrieZipper a -> TrieZipper a 
tryTo [] xs = xs
tryTo (firstChar : rest) this@(TrieZipper (Trie tries b, bs)) =
    case Map.lookup firstChar tries of
        Nothing -> this
        Just r -> 
            -- b in this is strange
            tryTo rest (TrieZipper (r, (Context ((firstChar, b), Map.delete firstChar tries)):bs))


complete :: Ord a => [a] -> Trie a -> [[a]]
complete [] (Trie tries wordEnd) =
    (if wordEnd then [[]] else []) ++
    concat [map (char :) (complete [] trie) | (char, trie) <- Map.toList tries]
complete (firstChar : rest) (Trie tries _) =
    maybe [] (map (firstChar :) . complete rest) (Map.lookup firstChar tries)
