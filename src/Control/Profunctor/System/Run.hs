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

module Control.Profunctor.System.Run where

import Control.Profunctor.Prelude

newtype Run q x y p a b = Run {run :: p (a,q x y) b }
newtype PreRun p a b q y x = PreRun {prerun :: Run q x y p a b}

instance Profunctor p => Profunctor (Run q x y p) where
  dimap fl fr (Run paqxyb) = Run $  dimap (first' fl) fr paqxyb

instance ( Profunctor p
         , Profunctor q
         ) => Profunctor (PreRun p a b q) where
  dimap fl fr (PreRun (Run paqxyb)) = PreRun $ Run $ lmap (second' f) paqxyb
    where
--    f :: q a b -> q x y -- (x -> a) (b -> y)
--    fl :: (b -> y)
      f = dimap fr fl

instance Sieve p f => Sieve (Run q x y p) (Compose ((->) (q x y) ) f) where
  sieve (Run paqxyb) a = Compose f
    where
      f qxy = sieve paqxyb (a,qxy)

instance Cosieve p f => Cosieve (Run q x y p) (Compose ((,) (q x y) ) f) where
  cosieve (Run paqxyb) (Compose (qxy,fa) ) = cosieve paqxyb (fmap (\a->(a,qxy)) fa)
