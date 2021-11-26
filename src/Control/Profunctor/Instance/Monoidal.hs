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

module Control.Profunctor.Instance.Monoidal where

-- copy https://bartoszmilewski.com/2018/02/20/free-monoidal-profunctors/

import Control.Profunctor.Prelude
import Control.Profunctor.Monoidal

import Data.Bifunctor

import Data.Void

import Data.Tuple

data FreeSpace p q s t = -- FreeAllAltMon (FreeMon p)
    SDoneP (s -> ()) (() -> t)
  | SMoreP (PDay p q s t)
  | SAllAltMoreP (PAllAltDay p q s t)
  | SAllAltDoneP (s -> Void) (Void -> t)

instance Profunctor (FreeSpace p q) where
  dimap fl fr (SDoneP f g) = SDoneP (f . fl) (fr . g)
  dimap fl fr (SMoreP pd) = SMoreP $ dimap fl fr pd
  dimap fl fr (SAllAltMoreP pd) = SAllAltMoreP $ dimap fl fr pd
  dimap fl fr (SAllAltDoneP f g) = SAllAltDoneP (f . fl) (fr . g)

instance ProfunctorFunctor (FreeSpace p) where
  promap f (SMoreP pd) = SMoreP $ promap f pd
  promap f (SAllAltMoreP pd) = SAllAltMoreP $ promap f pd
  promap f (SDoneP x y) = SDoneP x y
  promap f (SAllAltDoneP x y) = SAllAltDoneP x y

instance HPFunctor (FreeSpace p) where
  ddimap = dimap

type FreeCosmos p = FixH (FreeSpace p)

distribl :: (a,Either b c) -> Either (a,b) (a,c)
distribl (a,Left b) = Left (a,b)
distribl (a,Right b) = Right (a,b)

reDistribl :: Either (a,b) (a,c) -> (a,Either b c)
reDistribl (Left (a,b)) = (a,Left b)
reDistribl (Right (a,c)) = (a,Right c)

distribr :: (Either b c,a) -> Either (b,a) (c,a)
distribr (Left b,a) = Left (b,a)
distribr (Right c,a) = Right (c,a)

reDistribr :: Either (b,a) (c,a) -> (Either b c,a)
reDistribr (Left (b,a)) = (Left b,a)
reDistribr (Right (c,a)) = (Right c,a)

fun1 :: Either a (b,c) -> (Either a b, c)
fun1 (Right (b,c)) = (Right b, c)
fun1 (Left a) = 

--distribG :: Sum (,) Either a b -> Sum (,) Either

data GPDay p q f s t = forall a b c d.
  GPDay (p a b) (q c d) (f b d -> t) (s -> f a c)

consSpace :: (Monoidal q, AltMonoidal q)
          => GPDay p q (Sum (,) Either) a b -> q c d
          -> GPDay p q (Sum (,) Either) (Sum (,) Either a c) (Sum (,) Either d b)
consSpace (GPDay
            (pxy :: p a1 b1)
            (quv :: q c1 d1)
            (yva :: Sum (,) Either b1 d1 -> b)
            (bxu :: a -> Sum (,) Either a1 c1)
            :: GPDay p q (Sum (,) Either) a b)
          (qcd :: q c d) =
      GPDay pxy (quv >**< qcd) f g
  where
    f :: Sum (,) Either b1 (d1, d) -> Sum (,) Either d b
    f (L2 p) = L2 $ (swap . first' (yva . L2) . reassoc') p -- first' L2 bimap yva id
    f (R2 p) = R2 $ (swap . first' (yva . R2) . reassoc') p
    g :: Sum (,) Either a c -> Sum (,) Either a1 (c1, c)
    g = _b
{-}
instance Monoidal (FreeCosmos p) where
  punit = SDoneP
  (InH (SDoneP au ub)) >**< frcd = dimap snd (\d -> (ub (), d)) frcd
  (InH (SMoreP dayab)) >**< frcd = InH (SMoreP (cons dayab frcd))
-}
{-}

data FreeSpace p q s t = -- FreeAllAltMon (FreeMon p)
    SDoneP (s -> ()) (() -> t)
  | SMoreP (PDay p q s t)
  | SAllAltMoreP (PAllAltDay p q s t)
  | SAllAltDoneP (s -> Void) (Void -> t)

instance Monoidal (FreeSOP p) where
  punit = InH $ AllAltMoreP $ PAllAltDay fm (FixH AllAltDoneP)
    where
      fm = InH $ DoneP id id
  >**< =
-}

{-}
instance (Monoidal p,Monoidal q) => Monoidal (Sum p q) where
  punit = R2 punit
  >**<
-}
{-}
instance (AltMonoidal p, AltMonoidal q) => AltMonoidal (Sum p q) where
  pzero = undefined
  >||< =
-}
