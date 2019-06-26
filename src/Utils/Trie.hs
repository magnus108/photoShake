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


data Data a = Data (Map a (Trie a)) Bool deriving Show

data Trie a = Trie (Data a) deriving Show


data Context a = Context (Data a, Map a (Trie a)) deriving Show

data TrieZipper a = TrieZipper (Trie a, [Context a] ) deriving Show


up :: TrieZipper a -> TrieZipper a  
--up (TreeZipper (item, Context ls x rs:bs)) = TreeZipper (Tree x (ls ++ [item] ++ rs), bs)  
up (TrieZipper (_, Context (h, _):tl)) = TrieZipper (Trie h, tl)  
up root = root 


tryTo :: Ord a => [a] -> TrieZipper a -> TrieZipper a 
tryTo [] xs = xs
tryTo (firstChar : rest) this@(TrieZipper (Trie (Data tries b), context)) =
    case Map.lookup firstChar tries of
        Nothing -> this
        Just trie -> 
            tryTo rest (TrieZipper (trie, (Context (Data tries b, Map.delete firstChar tries )):context))


empty :: Ord a => Trie a
empty = Trie (Data Map.empty False)


insert :: Ord a => [a] -> Trie a -> Trie a
insert [] (Trie (Data tries _)) = Trie (Data tries True)
insert word@(firstChar : rest) (Trie (Data tries wordEnd)) =
    case Map.lookup firstChar tries of
        Nothing ->
            insert word (Trie (Data (Map.insert firstChar empty tries) wordEnd))
        Just trie ->
            Trie (Data (Map.insert firstChar (insert rest trie) tries) wordEnd)


complete :: Ord a => [a] -> Trie a -> [[a]]
complete [] (Trie (Data tries wordEnd)) =
    (if wordEnd then [[]] else []) ++
    concat [map (char :) (complete [] trie) | (char, trie) <- Map.toList tries]
complete (firstChar : rest) (Trie (Data tries _)) =
    maybe [] (map (firstChar :) . complete rest) (Map.lookup firstChar tries)
