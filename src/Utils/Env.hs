{-# LANGUAGE DeriveFunctor #-}  
module Utils.Env
    ( env
    )
  where

import Utils.Comonad

data Env e a = Env e a 
    deriving (Functor)


env :: e -> a -> Env e a
env = Env

instance Comonad (Env e) where
    extract (Env _ a)  = a
    duplicate env@(Env e a) = Env e env
