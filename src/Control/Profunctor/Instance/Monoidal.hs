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

import Control.Profunctor.Prelude
import Control.Profunctor.Monoidal

import Data.Bifunctor

import Data.Void

import Data.Tuple

data FreeSpace v f p q s t = -- FreeAllAltMon (FreeMon p)
    SGDoneP (s -> v) (v -> t)
  | SGMoreP (GPDay f p q s t)

-- GPDay (p a b) (q c d) (f b d -> t) (s -> f a c)
liftFreeCosmos :: p a b -> FreeCosmos (Maybe ()) Or p a b -- FreeProRing???????????
liftFreeCosmos pab = InH $ SGMoreP $ GPDay pab punit f g
  where
    f (OrAB x _ ) = x
    f (OrB _) = error "liftFreeCosmos:OrA"
    f (OrA x) = x
    g x = OrAB x ()


instance Profunctor (FreeSpace v f p q) where
  dimap fl fr (SGDoneP f g) = SGDoneP (f . fl) (fr . g)
  dimap fl fr (SGMoreP pd) = SGMoreP $ dimap fl fr pd

instance ProfunctorFunctor (FreeSpace v f p) where
  promap f (SGDoneP gr gl) = SGDoneP gr gl
  promap f (SGMoreP pd) = SGMoreP $ promap f pd

instance HPFunctor (FreeSpace v f p) where
  ddimap = dimap

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

--distribG :: Sum (,) Either a b -> Sum (,) Either

data GPDay f p q s t = forall a b c d.
  GPDay (p a b) (q c d) (f b d -> t) (s -> f a c)

instance Profunctor (GPDay f p q) where
  dimap fl fr (GPDay p q gr gl) = GPDay p q (fr . gr) (gl . fl)

instance ProfunctorFunctor (GPDay f p) where
  promap f (GPDay p q gr gl) = GPDay p (f q) gr gl

instance HPFunctor (GPDay f p) where
  ddimap = dimap

data Or a b = OrA a
            | OrB b
            | OrAB a b

assocOr :: Or (Or a b) c -> Or a (Or b c)
assocOr (OrAB (OrAB a b) c) = OrAB a (OrAB b c)
assocOr (OrAB (OrA a) c) = OrAB a (OrB c)
assocOr (OrAB (OrB b) c) = OrB (OrAB b c)
assocOr (OrA (OrAB a b)) = OrAB a (OrA b)
assocOr (OrA (OrA a)) = OrA a
assocOr (OrA (OrB b)) = OrB (OrA b)
assocOr (OrB c) = OrB (OrB c)
-- assocOr 0 = 0 ????

reassocOr :: Or a (Or b c) -> Or (Or a b) c
reassocOr (OrAB a (OrAB b c)) = OrAB (OrAB a b) c
reassocOr (OrAB a (OrA b)) = OrA (OrAB a b)
reassocOr (OrAB a (OrB c)) = OrAB (OrA a) c
reassocOr (OrA a) = OrA (OrA a)
reassocOr (OrB (OrAB b c)) = OrAB (OrB b) c
reassocOr (OrB (OrA b)) = OrA (OrB b)
reassocOr (OrB (OrB c)) = OrB c

firstOr :: (a -> b) -> Or a c -> Or b c
firstOr f (OrA a) = OrA (f a)
firstOr f (OrAB a c) = OrAB (f a) c
secondOr :: (c -> b) -> Or a c -> Or a b
secondOr f (OrB c) = OrB (f c)
secondOr f (OrAB a c) = OrAB a (f c)

swapOr :: Or a c -> Or c a
swapOr (OrAB a c) = OrAB c a
swapOr (OrA a) = OrB a
swapOr (OrB c) = OrA c

tuple2Or :: (a,b) -> Or a b
tuple2Or (a,b) = OrAB a b
or2Tuple (OrAB a b) = (a,b)
or2Tuple _ = error "not tuple"

either2Or :: Either a b -> Or a b
either2Or (Right b) = OrB b
either2Or (Left a) = OrA a
or2Either (OrAB a b) = Right b
or2Either (OrB b) = Right b
or2Either (OrA a) = Left a

consSpace :: (Monoidal q) -- MonoidalF p f
          => GPDay Or p q a b -> q c d
          -> GPDay Or p q (a,c) (b,d)
consSpace (GPDay -- GPDay (p a b) (q c d) (f b d -> t) (s -> f a c)
            (pxy :: p a1 b1)
            (quv :: q c1 d1)
            (yva :: Or b1 d1 -> b)
            (bxu :: a -> Or a1 c1)
            :: GPDay Or p q a b)
          (qcd :: q c d) =
      GPDay pxy (quv >**< qcd) f g
  where
    f :: Or b1 (d1, d) -> (b,d)
    f = or2Tuple . firstOr yva . reassocOr . secondOr tuple2Or
    g :: (a,c) -> Or a1 (c1 , c)
    g = secondOr or2Tuple . assocOr . firstOr bxu . tuple2Or

consSpaceAlt :: (AltMonoidal q) -- MonoidalF p f >##<
             => GPDay Or p q a b -> q c d
             -> GPDay Or p q (Either a c) (Either b d)
consSpaceAlt (GPDay -- GPDay (p a b) (q c d) (f b d -> t) (s -> f a c)
            (pxy :: p a1 b1)
            (quv :: q c1 d1)
            (yva :: Or b1 d1 -> b)
            (bxu :: a -> Or a1 c1)
            :: GPDay Or p q a b)
          (qcd :: q c d) =
      GPDay pxy (quv >||< qcd) f g
  where
    f :: Or b1 (Either d1 d) -> Either b d
    f = or2Either . firstOr yva . reassocOr . secondOr either2Or
    g :: Either a c -> Or a1 (Either c1 c)
    g = secondOr or2Either . assocOr . firstOr bxu . either2Or

type FreeCosmos v f p = FixH (FreeSpace v f p)
type FreeCosmosOr p = FreeCosmos (Maybe ()) Or p

instance Monoidal (FreeCosmosOr p) where
  punit = InH $ SGDoneP Just (const ())
  (InH (SGDoneP au ub)) >**< frcd = dimap snd (\d -> (ub (Just ()), d)) frcd
  (InH (SGMoreP dayab)) >**< frcd = InH (SGMoreP (consSpace dayab frcd))

instance AltMonoidal (FreeCosmosOr p) where
  pzero = InH $ SGDoneP (const Nothing) (const undefined)
  (InH (SGDoneP fv vf)) >||< frcd = dimap gl gr frcd
    where
      gl (Right c) = c
      gl (Left a) = error "FreeAllAltMon:FreeAllAltP:AltDoneP" -- ?????????????????
      gr = Right
  (InH (SGMoreP pad)) >||< frcd = InH $ SGMoreP $ consSpaceAlt pad frcd

{-
data FreeSpace v f p q s t = -- FreeAllAltMon (FreeMon p)
    SGDoneP (s -> v) (v -> t)
  | SGMoreP (GPDay f p q s t)
-}
