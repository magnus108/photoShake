module Utils.MultiwayTree
    ( Tree(..)
    , TreeZipper(..)
    , up
    , children
    , datum
    , tryTo
    ) where

-- a lens library would be cool

import Data.List (break)  


data Tree a = Tree a (Forest a) deriving Show

type Forest a = [Tree a]


datum :: Tree a -> a
datum (Tree d _) = d

children :: Tree a -> Forest a
children (Tree _ c) = c


data Context a = Context [Tree a] a [Tree a] deriving Show

data TreeZipper a = TreeZipper (Tree a, [Context a] ) deriving Show


up :: TreeZipper a -> TreeZipper a  
up (TreeZipper (item, Context ls x rs:bs)) = TreeZipper (Tree x (ls ++ [item] ++ rs), bs)  
up root = root 


simpleCompare :: Eq a => a -> Tree a -> Bool  
simpleCompare a t = a == datum t  


tryTo :: Eq a => a -> TreeZipper a -> TreeZipper a
tryTo x (TreeZipper (Tree item items, bs)) =   
    let (ls, r:rs) = break (simpleCompare x) items  
    in  TreeZipper (r, Context ls item rs:bs)  
