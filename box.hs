{-# LANGUAGE ExistentialQuantification, ExplicitForAll, RankNTypes #-}
module Diag where

import Prelude hiding (lookup, insert, (.), id)

import Control.Monad.ST
import Control.Monad.State
import Control.Arrow
import Control.Category
import Control.Applicative

import Data.Maybe
import Data.Vault.ST.Lazy as V


delta a = (a, a)

--type Diag s a = StateT (Vault s) (ST s) a
--data Link s a b = Link { unlink :: Maybe a -> Diag s (Maybe b) }

data Link s a b = Link { unlink :: ST s (a -> Diag s b) }
type Diag s a = State (Vault s) a

instance Functor (Link s a) where
  fmap f (Link stl) = Link $ do
    l <- stl
    return $ (\ a -> fmap f $ l a)

instance Applicative (Link s a) where
  pure b = Link $ return $ \ _ -> return b
  (Link stf) <*> (Link sta) = Link $ do
    fa <- sta
    ff <- stf
    return $ \ a -> do
      a' <- fa a
      f <- ff a
      return $ f a'

instance Category (Link s) where
  id = Link $ return (return)
  (Link stf) . (Link stg) = Link $ do
    f <- stf
    g <- stg
    return $ \ a -> g a >>= f

--TODO implement other parts of arrow instance
instance Arrow (Link s) where
  arr f = Link $ return $ \ a -> return $ f a
  first (Link stf) = Link $ do
    f <- stf 
    return $ \ (a, b) -> flip (,) b <$> f a

constantly a = arr (const a)

link :: (a -> e -> (b, e)) -> e -> Link s a b
link f def =
  let go key a e = do
      vault <- get
      let (b, e') = f a e
      put $ insert key e' vault
      return b
  in Link $ do
    key <- newKey
    return $ \ a -> do
      vault <- get
      case lookup key vault of
        Nothing -> do
          put (insert key def vault)
          go key a def
        Just e -> go key a e
--TODO write link' that takes an already allocated key.  
        
iter f def = link (\ a e -> delta $ f a e) def

type Source s b = Link s () b

producing f def = iter (\ _ e -> f def e) def

generations :: Source s Int
generations = producing (+) 0

runSource :: Source s b -> [b]
runSource (Link source) = runST $ do
  f <- source
  return $ evalState (sequence . repeat . f $ ()) V.empty 

{-

test :: Diag s Int
test = do
  l <- link (\ a b -> delta (a+b)) (0, 0)
  return 0
-}
{-  

instance Category (Link s) where
  id = Link undefined (return . const)
  (Link mk f) . (Link mk' f') = Link mk' f'' where
    f'' = do
      

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
-}
