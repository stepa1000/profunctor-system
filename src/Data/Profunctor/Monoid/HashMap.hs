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

module Data.Profunctor.Monoid.HashMap where

import Control.Profunctor.Postlude

import Data.Profunctor.System.Body

import Data.HashMap.Lazy
import Data.Hashable

import Control.Lens

--class Hashable h => WithHash a h | a -> h, h -> a where
--  getHashable :: a -> h  WithHash (p a b) h =>

data ProHashMap h p x y = ProHashMap
  {unProHashMap :: HashMap h (p x y)}

instance Profunctor p => Profunctor (ProHashMap h p) where
  dimap fl fr (ProHashMap hm) = ProHashMap $ fmap (dimap fl fr) hm

instance (Hashable h, Eq h) => ProSemigroup (ProHashMap h p) where
  (ProHashMap hm1) <<>> (ProHashMap hm2) = ProHashMap (hm1 <> hm2)

instance (Hashable h, Eq h) => ProMonoid (ProHashMap h p) where
  proMempty = ProHashMap mempty
  proMappend = (<<>>)
  proMconcat = Prelude.foldr (<<>>) proMempty

hositProHashMap :: (Eq h, Eq h2, Hashable h, Hashable h2,Profunctor p)
              => (HashMap h (p x y) -> HashMap h2 (p2 x2 y2))
              -> ProHashMap h p x y
              -> ProHashMap h2 p2 x2 y2
hositProHashMap f (ProHashMap hm) = ProHashMap $ f hm

proHashMapTo :: (Eq h)
             => (HashMap h (p x y) -> z)
             -> ProHashMap h p x y
             -> z
proHashMapTo f (ProHashMap hm) = f hm

singletonPHM :: (Eq h, Hashable h)
             => h
             -> p x y
             -> ProHashMap h p x y
singletonPHM h b = ProHashMap $ singleton h b

sizePHM :: (Eq h)
        => ProHashMap h p x y -> Int
sizePHM = proHashMapTo Data.HashMap.Lazy.size

lookupPHM :: (Hashable h, Eq h)
           => h -> ProHashMap h p x y -> Maybe (p x y)
lookupPHM h = proHashMapTo (Data.HashMap.Lazy.lookup h)

isoProHashMap :: (Eq h)
              => Iso' (ProHashMap h p x y) (HashMap h (p x y))
isoProHashMap = iso unProHashMap ProHashMap
