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

module Data.Profunctor.System.Arrow where

import qualified Prelude as CPre
import Prelude ( ($) )

import Control.Profunctor.Postlude

import Control.Category.Free

import Control.Category
import Control.Arrow

import Data.Either

newtype ProArrow p a b = ProArrow
  {runProArrow :: Queue (FreeCosmosOr (Sum (->) p)) a b}

execProArrow :: Category q -- (Arrow q, ArrowChoice q)
             => (forall x y. FreeCosmosOr (Sum (->) p) x y -> q x y)
             -> ProArrow p a b
             -> q a b
execProArrow f (ProArrow q) = foldNatQ f q

liftProArrow :: p a b -> ProArrow p a b
liftProArrow p = ProArrow $ ConsQ (liftFreeCosmos (R2 p) ) NilQ

instance Category (ProArrow p) where
  id = ProArrow NilQ
  ProArrow a . ProArrow b = ProArrow $ a . b

instance Arrow (ProArrow p) where
  arr bc = ProArrow $ ConsQ (liftFreeCosmos (L2 bc) ) NilQ
  first (ProArrow bc :: ProArrow p a b) = ProArrow $ n bc
    where
      n :: Queue (FreeCosmosOr (Sum (->) p)) x y -> Queue (FreeCosmosOr (Sum (->) p)) (x,c) (y,c)
      n (ConsQ p q) = ConsQ (p >**< liftFreeCosmos (L2 id)) (n q)
      n NilQ = NilQ
  second (ProArrow bc :: ProArrow p a b) = ProArrow $ n bc
    where
      n :: Queue (FreeCosmosOr (Sum (->) p)) x y -> Queue (FreeCosmosOr (Sum (->) p)) (c,x) (c,y)
      n (ConsQ p q) = ConsQ (liftFreeCosmos (L2 id) >**< p) (n q)
      n NilQ = NilQ
  (ProArrow ab :: ProArrow p a b) *** (ProArrow dc :: ProArrow p d c)
    = ProArrow $ n ab dc
    where
      n :: Queue (FreeCosmosOr (Sum (->) p)) x y
        -> Queue (FreeCosmosOr (Sum (->) p)) x' y'
        -> Queue (FreeCosmosOr (Sum (->) p)) (x,x') (y,y')
      n (ConsQ p q) (ConsQ p' q') = ConsQ (p >**< p') (n q q')
      n NilQ NilQ = NilQ
--  ab &&& dc =  ab *** dc

instance ArrowChoice (ProArrow p) where
  left (ProArrow ab :: ProArrow p a b) = ProArrow $ n ab
    where
      n :: Queue (FreeCosmosOr (Sum (->) p)) x y
        -> Queue (FreeCosmosOr (Sum (->) p)) (Either x c) (Either y c)
      n (ConsQ p q) = ConsQ (p >||< liftFreeCosmos (L2 id)) (n q)
      n NilQ = NilQ
  right (ProArrow ab :: ProArrow p a b) = ProArrow $ n ab
    where
      n :: Queue (FreeCosmosOr (Sum (->) p)) x y
        -> Queue (FreeCosmosOr (Sum (->) p)) (Either c x) (Either c y)
      n (ConsQ p q) = ConsQ (liftFreeCosmos (L2 id) >||< p) (n q)
      n NilQ = NilQ
  (ProArrow ab :: ProArrow p a b) +++ (ProArrow dc :: ProArrow p d c)
    = ProArrow $ n ab dc
    where
      n :: Queue (FreeCosmosOr (Sum (->) p)) x y
        -> Queue (FreeCosmosOr (Sum (->) p)) x' y'
        -> Queue (FreeCosmosOr (Sum (->) p)) (Either x x') (Either y y')
      n (ConsQ p q) (ConsQ p' q') = ConsQ (p >||< p') (n q q')
      n NilQ NilQ = NilQ
