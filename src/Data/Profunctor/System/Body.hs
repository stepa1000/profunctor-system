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

module Data.Profunctor.System.Body where

import Control.Profunctor.Postlude

newtype Body a b p x y = forall x' y'. Body -- ~Coyoneda
  { runBody :: p x' y'
  , toBody :: x -> a
  , toBody' :: a -> x'
  , fromBody :: b -> y
  , fromBody' :: y' -> b
  }

endoBody :: Bool -> (a -> a) -> (b -> b) -> Body a b p x y -> Body a b p x y
endoBody False fr fl (Body p t t' f f') = Body p (fr . t) t' f (fl . f')
endoBody True  fr fl (Body p t t' f f') = Body p t (t' . fr) (f . fl) f'

instance Profunctor (Body a b p) where
  dimap fl fr (Body p t t' f f') = Body p (fl . t) t' (f . fr) f'

instance ProfunctorFunctor (Body a b) where
  promap f (Body p t t' f f') = Body (f p) t t' f f'

instance HPFunctor (Body a b r) where
  ddimap = dimap

type Meat a b p = FixH (ProZero p (Body a b))

allMeat :: Bool -> (a -> a) -> (b -> b) -> Meat a b p x y -> Meat a b p x y
allMeat _ _ _ _ = undefined
