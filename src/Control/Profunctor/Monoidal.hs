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

module Control.Profunctor.Monoidal where

-- copy https://bartoszmilewski.com/2018/02/20/free-monoidal-profunctors/

import Control.Profunctor.Prelude

import Data.Bifunctor
import Data.Void -- VOID

class Monoidal p where
  punit :: p () ()
  (>**<) :: p a b -> p c d -> p (a, c) (b, d)

class AltMonoidal p where
  pzero :: p Void Void
  (>||<) :: p a b -> p c d -> p (Either a c) (Either b d)

class ProfunctorFunctor pp => HPFunctor pp where
  ddimap :: (s -> a) -> (b -> t) -> pp p a b -> pp p s t
  default ddimap :: Profunctor (pp p) => (s -> a) -> (b -> t) -> pp p a b -> pp p s t
  ddimap = dimap

data PDay p q s t = forall a b c d.
     PDay (p a b) (q c d) ((b, d) -> t) (s -> (a, c))

instance Profunctor (PDay p q) where
  dimap fl fr (PDay p q gr gl) = PDay p q (fr . gr) (gl . fl)

instance ProfunctorFunctor (PDay p) where
  promap f (PDay p q gr gl) = PDay p (f q) gr gl

instance HPFunctor (PDay p) where
  ddimap = dimap

newtype FixH pp a b = InH { outH :: pp (FixH pp) a b }

instance HPFunctor pp => Profunctor (FixH pp) where
    dimap f g (InH pp) = InH (ddimap f g pp)

data FreeP p q s t =
      DoneP (s -> ()) (() -> t)
    | MoreP (PDay p q s t)

instance Profunctor (FreeP p q) where
  dimap f g (DoneP au ub) = DoneP (au . f) (g . ub)
  dimap f g (MoreP day) = MoreP (dimap f g day)

instance ProfunctorFunctor (FreeP p) where
  promap _ (DoneP su ut) = DoneP su ut
  promap nat (MoreP day) = MoreP (promap nat day)

instance HPFunctor (FreeP p) where
  ddimap = dimap

type FreeMon p = FixH (FreeP p)

cons :: Monoidal q => PDay p q a b -> q c d -> PDay p q (a, c) (b, d)
cons (PDay pxy quv yva bxu) qcd =
      PDay pxy (quv >**< qcd) (bimap yva id . reassoc')
                              (assoc' . bimap bxu id)

assoc' ((a,b),c) = (a,(b,c))
reassoc' (a, (b, c)) = ((a, b), c)

instance Profunctor p => Monoidal (FreeMon p) where
  punit = InH (DoneP id id)
  (InH (DoneP au ub)) >**< frcd = dimap snd (\d -> (ub (), d)) frcd
  (InH (MoreP dayab)) >**< frcd = InH (MoreP (cons dayab frcd))

data PAltDay p q s t = forall a b c d.
  PAltDay (Either (p a b) (q c d)) (Either b d -> t) (s -> Either a c)

instance Profunctor (PAltDay p q) where
  dimap fl fr (PAltDay ep gr gl) = PAltDay ep (fr . gr) (gl . fl)

instance ProfunctorFunctor (PAltDay p) where
  promap f (PAltDay (Right q) gr gl) = PAltDay (Right $ f q) gr gl
  promap _ (PAltDay (Left p) gr gl) = PAltDay (Left p) gr gl

instance HPFunctor (PAltDay p) where
  ddimap = dimap

data FreeAltP p q s t =
    AltDoneP (s -> Void) (Void -> t) -- ?????????????????????????????????????????
  | AltMoreP (PAltDay p q s t)

altCons :: AltMonoidal q => PAltDay p q a b -> q c d -> PAltDay p q (Either a c) (Either b d)
altCons (PAltDay (Right quv) yva bxu) qcd =
      PAltDay (Right $ quv >||< qcd) (bimap yva id . altReassoc) -- _a
                                     (altAssoc . bimap bxu id) -- _b
--  where
--    fa (R)
altCons (PAltDay (Left p) yva bxu) _ = PAltDay (Left p) (Left . yva) s
  where
    s (Left a) = bxu a
    s _ = error "altCons:EEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEEError"

altAssoc :: Either (Either a b) c -> Either a (Either b c)
altAssoc (Left (Left a)) = Left a
altAssoc (Left (Right b)) = Right $ Left b
altAssoc (Right c) = Right $ Right c
altReassoc :: Either a (Either b c) -> Either (Either a b) c
altReassoc (Left a) = Left $ Left a
altReassoc (Right (Left b)) = Left $ Right b
altReassoc (Right (Right c)) = Right c

instance Profunctor (FreeAltP p q) where
  dimap f g (AltDoneP au ub) = AltDoneP (au . f) (g . ub)
  dimap f g (AltMoreP day) = AltMoreP (dimap f g day)

instance ProfunctorFunctor (FreeAltP p) where
  promap _ (AltDoneP su ut) = AltDoneP su ut
  promap nat (AltMoreP day) = AltMoreP (promap nat day)

instance HPFunctor (FreeAltP p) where
  ddimap = dimap

type FreeAltMon p = FixH (FreeAltP p)

instance Profunctor p => AltMonoidal (FreeAltMon p) where
  pzero = undefined
  (InH (AltDoneP fv vf)) >||< frcd = dimap gl gr frcd
    where
      gl (Right c) = c
      gl (Left a) = error "AltMonoidal:FreeAltMon:AltDoneP" -- ?????????????????
      gr = Right
  (InH (AltMoreP pad)) >||< frcd = InH $ AltMoreP $ altCons pad frcd
-- (>||<) :: FreeAltMon p a b -> FreeAltMon p c d -> FreeAltMon p (Either a c) (Either b d)

type FreeAltAndMon p = FreeAltMon (FreeMon p)
