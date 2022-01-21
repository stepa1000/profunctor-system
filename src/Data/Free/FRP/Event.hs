{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
--{-# LANGUAGE CPP                 #-}

module Data.Free.FRP.Event where

import qualified Prelude
import Prelude (const,($))

--import Control.Free.Algebra.Prelude
import Control.Profunctor.Postlude
import Data.Profunctor.Traversing

import Control.Category.Free
import Control.Arrow
import Control.Algebra.Free2
import Control.Category

import Control.Monad

import Data.Monoid
import Data.Either
import Data.Kind
import Data.Tuple

import Data.Semigroup as SG
import Data.Proxy

import Data.Free.Arrow
import Data.Free.ProFree

type family Event e :: * -> *
--type family SpaceEventIn p :: * -> *
--type family SpaceEventOut p :: * -> *

data ProEvent e x y where
  InitEvent :: ProFree (Proxy a) (Event e a) x y -> ProEvent e x y
  PutEvent :: ProFree (Event e a,a) () x y -> ProEvent e x y
  GetEvent :: ProFree (Event e a) a x y -> ProEvent e x y
  DupEvent :: ProFree (Event e a) (Event e a) x y -> ProEvent e x y
  ClonEvent :: ProFree (Event e a) (Event e a) x y -> ProEvent e x y
--  deriving Profunctor -- ,ProfunctorFunctor,HPFunctor)
--    via (ProFree * *)

instance Profunctor (ProEvent e) where
  dimap fl fr (InitEvent p) = InitEvent $ dimap fl fr p
  dimap fl fr (PutEvent p) = PutEvent $ dimap fl fr p
  dimap fl fr (GetEvent p) = GetEvent $ dimap fl fr p
  dimap fl fr (DupEvent p) = DupEvent $ dimap fl fr p
  dimap fl fr (ClonEvent p) = ClonEvent $ dimap fl fr p

class ProfunctorEvent e p | p -> e where
  initEvent :: p (Proxy a) (Event e a)
  putEvent :: p (Event e a,a) ()
  getEvent :: p (Event e a) a
  dupEvent :: p (Event e a) (Event e a)
  cloneEvent :: p (Event e a) (Event e a)

instance ProfunctorEvent e (ProEvent e) where
  initEvent = InitEvent $ idProFree
  putEvent = PutEvent $ idProFree
  getEvent = GetEvent $ idProFree
  dupEvent = DupEvent $ idProFree
  cloneEvent = ClonEvent $ idProFree
