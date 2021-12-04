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

class Hashable h => WithHash a h | a -> h, h -> a where
  getHashable :: a -> h

data ProHashMap h a b p x y = WithHash (p a b) h => ProHashMap
  {unProHashMap :: HashMap h (Body a b p x y)}

instance Profunctor (ProHashMap h a b p) where
  dimap fl fr (ProHashMap hm) = ProHashMap $ fmap (dimap fl fr) hm

instance Eq h => ProSemigroup (ProHashMap h a b p) where
  (ProHashMap hm1) <<>> (ProHashMap hm2) = ProHashMap (hm1 <> hm2)

instance (Eq h, WithHash (p a b) h) => ProMonoid (ProHashMap h a b p) where
  proMempty = ProHashMap mempty
  proMappend = (<<>>)
  proMconcat = Prelude.foldr (<<>>) proMempty

hositProHashMap :: (WithHash (p a b) h, Eq h,WithHash (p2 a2 b2) h2, Eq h2, Hashable h, Hashable h2)
              => (HashMap h (Body a b p x y) -> HashMap h2 (Body a2 b2 p2 x2 y2))
              -> ProHashMap h a b p x y
              -> ProHashMap h2 a2 b2 p2 x2 y2
hositProHashMap f (ProHashMap hm) = ProHashMap $ f hm

proHashMapTo :: (WithHash (p a b) h, Eq h)
             => (HashMap h (Body a b p x y) -> z)
             -> ProHashMap h a b p x y
             -> z
proHashMapTo f (ProHashMap hm) = f hm

singletonPHM :: (WithHash (p a b) h, Eq h, Hashable h)
             => h
             -> Body a b p x y
             -> ProHashMap h a b p x y
singletonPHM h b = ProHashMap $ singleton h b

sizePHM :: (WithHash (p a b) h, Eq h)
        => ProHashMap h a b p x y -> Int
sizePHM = proHashMapTo Data.HashMap.Lazy.size

lookupPHM :: (WithHash (p a b) h, Eq h)
           => h -> ProHashMap h a b p x y -> Maybe (Body a b p x y)
lookupPHM h = proHashMapTo (Data.HashMap.Lazy.lookup h)

isoProHashMap :: (WithHash (p a b) h, Eq h)
              => Iso' (ProHashMap h a b p x y) (HashMap h (Body a b p x y))
isoProHashMap = iso unProHashMap ProHashMap
