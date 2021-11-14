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

import Control.Profunctor.Prelude

newtype Interact s p x y = Interact {interact :: p (x,s) (y,s)}

instance Profunctor p => Profunctor (Interact s p) where
  dimap fl fr (Interact p) = Interact $ dimap (first' fl) (first' fr) p

data FixPro p a b = FixPro {unFixPro :: p (FixPro p) a b}

instance Profunctor (p (FixPro p)) => Profunctor (FixPro p) where
  dimap fl fr (FixPro pp) = FixPro $ dimap fl fr pp -- RECURSION PROFUNCTOR

data ProEither (p :: * -> * -> *) (q :: * -> * -> *) a b = ProLeft (p a b)
                                                         | ProRight (q a b)

newtype ProZero p (pn :: (* -> * -> *) -> * -> * -> *) fp a b = ProZero
  {unProZero :: ProEither (pn fp) p a b}

instance ( Profunctor p
         , Profunctor (pn fp)
         ) => Profunctor (ProZero p pn fp) where
  dimap fl fr (ProZero (ProLeft ipf)) = ProZero $ ProLeft $ dimap fl fr ipf
  dimap fl fr (ProZero (ProRight ipf)) = ProZero $ ProRight $ dimap fl fr ipf

newtype FixProZero (p :: * -> * -> *) (pn :: (* -> * -> *) -> * -> * -> *) a b = FixProZero
  {unFixProZero :: FixPro (ProZero p pn) a b}
{-}
instance ( Profunctor p
         , Profunctor (ProZero p pn (FixPro ) )
         )
-}
newtype Interacting' s p fp x y = Interacting'
  {unInteracting' :: ProEither (Interact s fp) p x y}

instance Profunctor p
  => Profunctor (Interacting' s p (FixPro (Interacting' s p) )) where
  dimap fl fr (Interacting' (ProLeft ipf)) = Interacting' $ ProLeft $ dimap fl fr ipf
  dimap fl fr (Interacting' (ProRight p)) = Interacting' $ ProRight $ dimap fl fr p

newtype Interacting s p x y = Interacting {unInteracting :: FixPro (Interacting' s p) x y}

instance Profunctor p => Profunctor (Interacting s p) where
  dimap fl fr (Interacting ip) = Interacting $ dimap fl fr ip
{-}
getInteract' :: Interacting s p x y -> Interacting' s (Interacting s p x y) x y
getInteract' inter =

down :: Profunctor p
     => Int
     -> (Interacting s p x y -> Interacting s x a y)
     -> Interacting s p x y -> Interacting s x a y
down i f (Interacting a) = Interacting $
  where
    g 0 n = f n
    g j (FixPro (ProLeft n) ) | i > 0 = Interacting $ FixPro $ ProLeft $ g (j-1) n
-}
--lmodifyInt :: Int -> ((x,s) -> (x,s)) -> Interacting s p x y -> Interacting s x a y
--lmodifyInt i fl =
{-}
newtype ProFold a p x y = ProFold {unProFold :: p (x,a) y}

instance Profunctor p => Profunctor (ProFold a p) where
  dimap fl fr (ProFold p) = ProFold $ dimap (first' fl) fr p

newtype ProFolding' a p fp x y = ProFolding'
  {unProFolding' :: ProZero p (ProFold a) fp x y


instance ( Profunctor p
         , Profunctor (FixPro (ProFolding' a p) )
         ) => Profunctor (ProFolding' a p (FixPro (ProFolding' a p) ) ) where
  dimap fl fr (ProFolding' (ProZero (ProLeft fp ))) = ProFolding' $
    ProZero $ ProLeft $ dimap fl fr fp

newtype ProFolding a p x y = ProFolding {unProFolding :: FixPro (ProFolding' a p) x y}

instance Profunctor p => Profunctor (ProFolding a p) where
  dimap fl fr (ProFolding p) = ProFolding $ dimap fl fr p


data Attitude s a b p x y = Attitude
  { attitude :: p a b
  , toAtt :: (x,s) -> a
  , fromAtt :: b -> (y,s)
  }

instance Profunctor p => Profunctor (Attitude s a b p) where
  dimap fl fr (Attitude att toA fromA ) =
-}
