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

module Control.Profunctor.MonoidalAssoc where

import Control.Profunctor.Prelude

import Data.Bifunctor
import Data.Void -- VOID

data Or a b = OrA a
            | OrB b
            | OrAB a b

instance Functor (Or a) where
  fmap f (OrA a) = OrA a
  fmap f (OrB b) = OrB (f b)
  fmap f (OrAB a b) = OrAB a (f b)

class Associative f where
  fassoc :: f (f a b) c -> f a (f b c)
  freAssoc :: f a (f b c) -> f (f a b) c

instance Associative (,) where
  fassoc ((a,b),c) = (a,(b,c))
  freAssoc (a,(b,c)) = ((a,b),c)
{-}
instance Associative Either where
  fassoc = altAssoc
  freAssoc = altReassoc
-}
instance Associative Or where
  fassoc = assocOr
  freAssoc = reassocOr
{-}
class Associative f => MonoidalAssoc p f e | f -> e where
  oneMA :: p e e
  (>##<) :: p a b -> p c d -> p (f a c) (f b d)

data FreeSpace v f p q s t = -- FreeAllAltMon (FreeMon p)
    SGDoneP (s -> v) (v -> t)
  | SGMoreP (GPDay f p q s t)

data GPDay f p q s t = forall a b c d.
  GPDay (p a b) (q c d) (f b d -> t) (s -> f a c)

consAssoc :: (Associative f, MonoidalAssoc q f e)
          => GPDay f p q a b
          -> q c d
          -> GPDay f p q (f a c) (f b d)
consAssoc (GPDay pxy quv yva bxu) qcd =
      GPDay pxy (quv >##< qcd) (bimap yva id . freAssoc) -- _a
                                         (fassoc . bimap bxu id) -- _b
-}

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
