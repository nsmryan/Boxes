{-# LANGUAGE ExistentialQuantification, ExplicitForAll, RankNTypes #-}
module Diag where

import Prelude hiding (lookup, insert, (.))

import Control.Monad.ST
import Control.Monad.State
import Control.Arrow
import Control.Category

import Data.Maybe
import Data.Vault.ST.Lazy


data Link s a b =
  forall e. Link
    {
       linkKey :: Key s (b, e)
    ,  pushLink :: (a, e) -> (b, e)
    }


instance Category (Link s) where
  id = Link undefined id 
  (Link k f) . (Link k' f') = 

type Diag s a = State (Vault s) a

pull :: Link s a b -> Diag s b
pull (Link k f) = do
  v <- get
  return . fst . fromJust . lookup k $ v

push :: a -> Link s a b -> Diag s b
push a (Link k f) = do
  v <- get
  let (b, e) = f . (first (const a)) . fromJust . lookup k $ v
  modify (insert k (b, e))
  return b

evalDiag :: Vault s -> Diag s a -> a
evalDiag v m = evalState m v

runDiag :: Vault s -> Diag s a -> (a, Vault s)
runDiag v m = runState m v

runWith :: Diag s a -> Vault s -> a
runWith = flip runDiag

main = undefined

