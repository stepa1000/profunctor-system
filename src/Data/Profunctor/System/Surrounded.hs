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

module Data.Profunctor.System.Surrounded where

import Control.Profunctor.Postlude

data Surrounding e p x y = Surrounding e (p x y)

class Surrounded p e | p -> e where
  setEnv :: e -> p x y -> p x y
  getEnv :: p x y -> e

instance Profunctor p => Profunctor (Surrounding e p) where
  dimap fl fr (Surrounding e p) = Surrounding e $ dimap fl fr p

instance ProfunctorFunctor (Surrounding e) where
  promap f (Surrounding e p) = Surrounding e $ f p

instance HPFunctor (Surrounding e) where
  ddimap = dimap

instance Surrounded (Surrounding e p) e where
  setEnv e (Surrounding _ p) = Surrounding e p
  getEnv (Surrounding e _) = e
