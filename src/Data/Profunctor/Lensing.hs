{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE Arrows #-}

module Data.Profunctor.Lensing where

import GHC.Generics

--import SystemGame.Generic.GetSet

import Control.Arrow
import Control.Monad
import Control.Monad.Fix
import Control.Comonad

import Control.Monad.State.Lazy
import Control.Comonad.Store

import Control.Lens
import Control.Lens.Internal.Getter

import Data.Profunctor
import Data.Profunctor.Monad
import Data.Bifunctor.Functor

import Data.Profunctor.Strong
import Data.Profunctor.Closed
import Data.Profunctor.Choice
import Data.Profunctor.Traversing

import Data.Profunctor.Mapping
import Data.Profunctor.Yoneda
import Data.Bifunctor.Sum
import Data.Bifunctor.Product

import Data.Bifunctor.Tannen
import Data.Profunctor.Composition

import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Bifunctor

import Data.Profunctor.Object

class Profunctor p => Lensing p s t | p -> s, p -> t where
  lensing :: Functor f => p b a -> (a -> f b) -> s -> f t

instance Lensing (LensObj s t) s t where
  lensing (LensObj le) = le

instance ( Lensing p a0 o
         , Profunctor p
         ) => Lensing (Cotambara p) a0 o where
  lensing = lensing . proextract

instance ( Lensing p a0 o
         , Profunctor p
         ) => Lensing (Tambara p) a0 o where
  lensing = lensing . proextract

instance ( Lensing p a0 o
         , Profunctor p
         ) => Lensing (Closure p) a0 o where
  lensing = lensing . proextract

instance ( Lensing p a0 o
         , Profunctor p
         ) => Lensing (TambaraSum p) a0 o where
  lensing = lensing . proextract

instance Lensing p a0 o => Lensing (CofreeTraversing p) a0 o where
  lensing = lensing . proextract

instance Lensing p a0 o => Lensing (CofreeMapping p) a0 o where
  lensing = lensing . proextract

instance Lensing p a0 o => Lensing (Coyoneda p) a0 o where
  lensing = lensing . proextract

instance Lensing p a0 o => Lensing (Yoneda p) a0 o where
  lensing = lensing . proextract

instance ( Lensing p a0 o
         , Lensing q a0 o
         ) => Lensing (Sum p q) a0 o where
  lensing (L2 p) = lensing p
  lensing (R2 p) = lensing p

instance ( Lensing p a0 o
         , Lensing q a0 o
         ) => Lensing (Product p q) a0 o where
  lensing = lensing . biextract

instance ( Lensing p a0 o
         , Comonad f
         ) => Lensing (Tannen f p) a0 o where
  lensing = lensing . biextract
{-}
instance ( Lensing p t s
         , Lensing q s t
         ) => Lensing (Procompose p q) s t where
  lensing (Procompose p q) f s0 = mt -- ( . (\x-> set lep x s) . (^.leq) ) s
    where
      mx = case ms of
        Just s -> return $ s^.leq
        Nothing -> return $ s0^.leq
      mc = fmap (^.lep) (mt sn)
      mt = case ms of
        Nothing -> mx >>= (\a -> set leq a s0)
        Just sx -> do
          x <- mx
          return $ set leq x sx
      ms = do
        t <- mt
        c <- mc
        return $ set lep (f c) t
      leq = lensing q
      lep = lensing p
-- Procompose p q b c = p x c -> q b x
-- q b x -> (x -> f b) -> s -> f t
-- p x c -> (c -> f x) -> t -> f s
-- f :: c -> f b
--lensing :: Functor f => p b a -> (a -> f b) -> s -> f t
-- p x b -> q x a -> (a -> f b) -> s -> f t
-}

class ( Profunctor p
      ) => LensingM p s t | p -> s, p -> t where
  lensingM :: MonadFix m => p b a -> (a -> m b) -> s -> m t

newtype PureLensing p a b = PureLensing
  {unPureLensing :: p a b} deriving (Profunctor)

instance Lensing p s t => LensingM (PureLensing p) s t where
  lensingM (PureLensing p) = lensing p

instance LensingM p s t => LensingM (Pastro p) s t where
  lensingM (Pastro gb p ag) f = lensingM p (runKleisli g)
    where
--    g :: (y -> m x)
      g = loop $ proc (y,z) -> do
          out <- Kleisli f -< gb (y,z)
          returnA -< ag out
--lensing :: Functor f => p b a -> (a -> f b) -> s -> f t
-- Comp1

class Profunctor p => Objecting p a b | p -> a, p -> b where
  objecting :: Functor f => p s t -> (a -> f b) -> s -> f t

instance Objecting (ObjLens a b) a b where
  objecting (ObjLens le) f = le f

--instance
