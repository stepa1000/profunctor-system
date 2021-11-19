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

module Data.Profunctor.System.Interact where

import Control.Profunctor.Postlude

class Interacted p s | p -> s where
  interacted :: (a -> s -> (x,s)) -> (y -> s -> (b,s)) -> p x y -> p a b
  copuSt :: p x y -> p x (y,s)
  copuSt = interacted (\x s->(x,s)) (\y s->((y,s),s) )

instance Profunctor p => Interacted (Interact s p) s where
  interacted stx sty (Interact p) = Interact $ dimap (uncurry stx) (uncurry sty) p
--  interated (Interact p) = dimap (\(x,s)->((x,s),s) ) () p
--  unInterated (Interact p) = dimap

newtype InterList p a b = InterList {unInterList :: p a b}
newtype InterHead p a b = InterHead {unInterHead :: p a b}

instance Profunctor p => Interacted (InterList (Interacting s p)) [s] where
  interacted stl str = InterList . fromListInteracting . interacted stl str . toListInteracting . unInterList

instance Profunctor p => Interacted (InterHead (Interacting s p)) s where
  interacted stl str (InterHead (InH (ProZero (ProLeft p) ) )) = InterHead $
    InH $ ProZero $ ProLeft $ interacted stl str p

newtype Interact s p x y = Interact {unInteract :: p (x,s) (y,s)}

instance Profunctor p => Profunctor (Interact s p) where
  dimap fl fr (Interact p) = Interact $ dimap (first' fl) (first' fr) p

instance ProfunctorFunctor (Interact s) where
  promap f (Interact p) = Interact $ f p

instance HPFunctor (Interact s) where
  ddimap = dimap

type Interacting s p = FixH (ProZero p (Interact s))

-- isoListInteracting ::

toListInteracting :: Profunctor p => Interacting s p x y -> ProWith Int (Interact [s] p) x y
toListInteracting = proCata f
  where
    f (ProZero (ProLeft (Interact a ) )) = mapProWith (+1) $ interacted fl fr a
      where
        fl x (s:ls) = ((x,s) ,ls)
        fr (y,s) ls = (y,s:ls)
    f (ProZero (ProRight a )) = ProWith 0 $ Interact $ dimap fl fr a
      where
        fl = fst
        fr y = (y,[])

type instance ProBase (FixH (ProZero p (Interact s))) = ProZero p (Interact s)

fromListInteracting :: Profunctor p => ProWith Int (Interact [s] p) x y -> Interacting s p x y
fromListInteracting = proAna f
  where
    f (ProWith i (p :: Interact [s] p x y) )
      | i > 1 =  ProZero $ ProLeft $ Interact $ ProWith (i-1) $ interacted fl fr p
      | i == 0 = ProZero $ ProRight $ dimap lf rf $ unInteract p
      | i < 1 = ProZero $ ProLeft $ Interact $ ProWith (-42) $ interacted fl fr p
        where
          fl :: (x,s) -> [s] -> (x,[s])
          fl (x,s) ls = (x,s:ls)
          fr y (s:ls) = ((y,s),ls)
          lf x = (x,[])
          rf (y,[]) = y
          rf _ = error "setListInteracting:f:rf"

data ProWith a p x y = ProWith a (p x y)
unProWith (ProWith a p) = (a,p)

mapProWith :: (a -> b) -> ProWith a p x y -> ProWith b p x y
mapProWith f (ProWith a p) = ProWith (f a) p

instance Profunctor p => Profunctor (ProWith a p) where
  dimap fl fr (ProWith a p) = ProWith a $ dimap fl fr p

instance Interacted p s => Interacted (ProWith a p) s where
  interacted fr fl (ProWith a p) = ProWith a $ interacted fr fl p
