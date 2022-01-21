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
{-# LANGUAGE DataKinds #-}

module Data.Profunctor.Monoid.Vec where

import Control.Profunctor.Postlude

import Data.Profunctor.System.Body

--import Data.HashMap.Lazy
--import Data.Hashable
import Data.Vec.Lazy as V

import Data.Type.Nat

import Control.Lens

data ProVec (n :: Nat) p x y = ProVec
  {unProVec :: Vec n (p x y)}

newtype ProVecN p x y n = VecN
  {unProVecN :: ProVec n p x y }

isoProVec :: Iso' (ProVec n p x y) (Vec n (p x y))
isoProVec = iso unProVec ProVec
{-}
data NProVec p x y = forall (n :: Nat). SNatI n => NProVec
  { unNProVec :: ProVec n p x y
  , snatNPV :: SNat n
  }
-}
instance Profunctor p => Profunctor (ProVec (n :: Nat) p) where
  dimap fl fr (ProVec v) = ProVec $ fmap (dimap fl fr) v

instance ProfunctorFunctor (ProVec (n :: Nat)) where
  promap f (ProVec v) = ProVec $ fmap f v

--instance (Profunctor p,) => Profunctor (ProVec n p) where

{-}
instance Profunctor p => Profunctor (NProVec p) where
  dimap fl fr (NProVec v s) = NProVec (dimap fl fr v) s
instance ProSemigroup (NProVec p) where -- (Plus n1 n2)
  (NProVec (ProVec (v1 :: Vec n1 (p x y))) (sn1 :: SNat n1) ) <<>>
    (NProVec (ProVec (v2 :: Vec n2 (p x y))) (sn2 :: SNat n2) )
    = ( NProVec :: forall (n3 :: Nat). (SNatI n1, SNatI n2
                  , SNatI n3, n3 ~ (Plus n1 n2), SNatI (Plus n1 n2))
                => ProVec n3 p x y -> SNat n3 -> NProVec p x y
      ) (ProVec ( (v1 V.++ v2)
        :: (SNatI n1, SNatI n2, SNatI n3, n3 ~ (Plus n1 n2), SNatI (Plus n1 n2))
          => Vec (Plus n1 n2) (p x y) ))
      (withSNat sn1 (withSNat sn2 snat)
        :: (SNatI n1, SNatI n2, SNatI n3, n3 ~ (Plus n1 n2)) => SNat n3 )
-}
