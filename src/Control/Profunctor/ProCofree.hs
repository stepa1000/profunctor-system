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
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Profunctor.ProCofree where

import Control.Profunctor.Prelude
import Control.Profunctor.Monoidal

import Data.Coerce
{-}
data Total a b p x y = Total {unTotal :: (a ~ x, b ~ y) => p x y}

data ProCofree s t (p :: (* -> * -> *) -> * -> * -> *) (a :: * -> * -> *) x y =
  ProCofree (   PDay (Total s t a) (  Total s t (p (ProCofree p a) )  ) x y   )
    (s -> (s,s)) ((t,t) -> t)

instance ( Profunctor a
         , HPFunctor p) => Profunctor (ProCofree p a) where
  dimap fl fr (ProCofree pd) = ProCofree $ dimap fl fr pd -- ????????????????????????

instance (ProfunctorFunctor p, HPFunctor p) => ProfunctorFunctor (ProCofree p) where
  promap (f :: p1 :-> q) (ProCofree pd) = ProCofree $ g pd
    where
      g :: PDay p1 (p (ProCofree p p1)) :-> PDay q (p (ProCofree p q))
      g (PDay p1 p gr gl) = PDay (f p1) (promap (promap f) p) gr gl

instance ProfunctorComonad (ProCofree p) where
  proextract
-}
