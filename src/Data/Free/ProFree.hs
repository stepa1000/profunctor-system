{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Arrows #-}
--{-# LANGUAGE CPP                 #-}

module Data.Free.ProFree where

import qualified Prelude
import Prelude (const)

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

import Data.Free.Arrow

idProFree :: ProFree a b a b
idProFree = ProFree id id

idProFreeT :: p a b -> ProFreeT a b p a b
idProFreeT p = ProFreeT id p id

data ProFree a b x y where
  ProFree :: (x -> a) -> (b -> y) -> ProFree a b x y

data ProFreeT a b p x y where
  ProFreeT :: (x -> a) -> p a b -> (b -> y) -> ProFreeT a b p x y

instance Profunctor (ProFree a b) where
  dimap fl fr (ProFree gl gr) = ProFree (gl . fl) (fr . gr)

instance Profunctor (ProFreeT a b p) where
  dimap fl fr (ProFreeT gl p gr) = ProFreeT (gl . fl) p (fr . gr)

instance ProfunctorFunctor (ProFreeT a b) where
  promap f (ProFreeT gl p gr) = ProFreeT gl (f p) gr

instance HPFunctor (ProFreeT a b) where
  ddimap = dimap
