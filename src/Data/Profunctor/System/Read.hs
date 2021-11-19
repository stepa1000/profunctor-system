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

module Data.Profunctor.System.Read where

import Control.Profunctor.Postlude

class Reader p r | p -> r where
  reader :: (a -> r -> (x,r)) -> p x y -> p a y

instance Profunctor p => Reader (ProRead r p) r where
  reader f = ProRead . lmap (uncurry f) . unProRead

--instance

newtype ProRead r p x y = ProRead {unProRead :: p (x,r) y}

instance Profunctor p => Profunctor (ProRead r p) where
  dimap fl fr = ProRead . dimap (first' fl) fr . unProRead

instance ProfunctorFunctor (ProRead r) where
  promap f (ProRead p) = ProRead $ f p

instance HPFunctor (ProRead r) where
  ddimap = dimap

type Reading r p = FixH (ProZero p (ProRead r))
