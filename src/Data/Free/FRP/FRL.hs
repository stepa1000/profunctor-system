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
{-# LANGUAGE FlexibleContexts #-}

module Data.Free.FRP.FRL where

import qualified Prelude
import Prelude
  (const,($),Traversable,Foldable,Int,IO)

--import Control.Free.Algebra.Prelude
import Control.Profunctor.Postlude
import Data.Profunctor.Traversing
import Data.Profunctor.Sieve

import Control.Category.Free
import Control.Arrow
import Control.Algebra.Free2
import Control.Category

import Control.Monad
import Control.Concurrent.Async

import Data.Monoid
import Data.Either
import Data.Kind
import Data.Tuple

import Data.Semigroup as SG
import Data.Proxy

import Data.Free.Arrow
import Data.Free.ProFree

data FRLf proNoda x y where
  FRLConcurIO :: proNoda x a -> proNoda x b -> FRLf proNoda x (a,b)
  FRLRaceIO :: proNoda x a -> proNoda x b -> FRLf proNoda x (Either a b)
  FRLMapConcurIO :: Traversable t => proNoda x y -> FRLf proNoda (t x) (t y)
  FRLMapConcurIO_ :: Foldable t => proNoda x () -> FRLf proNoda (t x) ()
  FRLRepliConcurIO :: Int -> proNoda x b -> FRLf proNoda x [b]
  FRLine :: proNoda x y -> FRLf proNoda x y
  FRLWait :: proNoda x (Async y) -> FRLf proNoda x y
--  FRLCoRepliIO :: Int -> proNoda x b -> FRLf proNoda (t x) b

{-
FRLineAA :: proNoda x y -> (Async x -> proNoda x y -> IO (Async y)) -> FRLf proNoda x y
FRLineAAny ::
-}

type FRL proNoda = C (FRLf proNoda)

frl :: Sieve proNoda IO
            => FRL proNoda x y -> Kleisli IO x y
frl = foldNatFree2 f
  where
--    f :: FRLf proNoda x y -> Kleisli IO x y
    f (FRLConcurIO p1 p2) = Kleisli $ \ x ->
      concurrently (sieve p1 x) (sieve p2 x)
    f (FRLRaceIO p1 p2) = Kleisli $ \ x ->
      race (sieve p1 x) (sieve p2 x)
    f (FRLMapConcurIO p) = Kleisli $ mapConcurrently (sieve p)
    f (FRLMapConcurIO_ p) = Kleisli $ mapConcurrently_ (sieve p)
    f (FRLRepliConcurIO i p) = Kleisli $ \ r -> replicateConcurrently i (sieve p r)
    f (FRLine p) = Kleisli (sieve p)
    f (FRLWait p) = Kleisli $ \x-> wait =<< (sieve p x)

type ProFRL proNoda a b = ProFreeT a b (FRL proNoda)

instance Sieve proNoda IO => Sieve (ProFRL proNoda a b) IO where
  sieve (ProFreeT fl p fr) = runKleisli (dimap fl fr (frl p))


{-}
class ToArrIO p where
  toArrIO :: p x y -> x -> IO y
-}
