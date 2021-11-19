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

import Control.Profunctor.ProTuple

import Data.Coerce

class ( HPFunctor p
      , HPFunctor pwf
      , ProfunctorFunctor p
      , ProfunctorComonad pwf
      ) => ProComonadCofree p pwf | pwf -> p where
  proUnwrap :: pwf a :-> p (pwf a)

instance( HPFunctor pp
        , ProfunctorFunctor pp
        ) => ProComonadCofree pp (ProCofree pp) where
  proUnwrap (ProCofree (PProTuple (p :^ pp))) = pp

newtype ProCofree pp p a b = ProCofree
  {unProCofree :: PProTuple p pp (ProCofree pp p) a b
  }

instance ( Profunctor p
         , HPFunctor pp
         ) => Profunctor (ProCofree pp p) where
  dimap dl dr (ProCofree ppt) = ProCofree $ dimap dl dr ppt

instance (HPFunctor pp,ProfunctorFunctor pp) => ProfunctorFunctor (ProCofree pp) where
  promap f (ProCofree (PProTuple (p :^ pp) )) = ProCofree $ PProTuple (f p :^ promap (promap f) pp)

instance HPFunctor pp => HPFunctor (ProCofree pp) where
  ddimap = dimap

instance HPFunctor pp => ProfunctorComonad (ProCofree pp) where
  proextract (ProCofree (PProTuple (p :^ pp) )) = p
  produplicate (ProCofree (PProTuple (p :^ pp) )) = ProCofree $ PProTuple (w :^ promap produplicate pp)
    where
      w = ProCofree $ PProTuple (p :^ pp)

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
