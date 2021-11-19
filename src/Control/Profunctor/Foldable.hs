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
{-# LANGUAGE AllowAmbiguousTypes #-}

module Control.Profunctor.Foldable where

import Control.Profunctor.Prelude

import Control.Profunctor.Monoidal
import Control.Profunctor.ProCofree
import Control.Profunctor.ProTuple

import Data.Coerce

type instance ProBase (FixH pp) = pp

instance ProfunctorFunctor pp => ProRecursive (FixH pp) where
  proProject (InH ppf) = ppf

instance ProfunctorFunctor pp => ProCorecursive (FixH pp) where
  proEmbed = InH

type family ProBase (p :: * -> * -> *) :: (* -> * -> *) -> * -> * -> *

type ProArrow' p q x y = p x y -> q x y
--prerunProArrow' :: ProArrow' p q x y -> (p :-> q)
--prerunProArrow' (ProArrow' f) = f

data ProCoercible p a b = forall x y. (Coercible a x, Coercible y b) => ProCoercible
  {unProCoercible :: p x y}

class ProfunctorFunctor (ProBase p) => ProRecursive (p :: * -> * -> *) where
  proProject :: p :-> ProBase p p -- ProArrow' p (ProBase p p) x y

proCata :: ( Profunctor p
           , ProRecursive p
           ) => (ProBase p a :-> a) -> (p :-> a)
proCata (f :: ProBase p a :-> a) = c
  where
    c :: p :-> a
    c p = f $ promap c $ proProject p


proGcata :: ( Profunctor t
            , ProRecursive t
            , ProfunctorComonad w
            , Profunctor (ProBase t (w a))
            , Profunctor a
            , Profunctor (w a)
            )
         => (forall b. Profunctor b => ProBase t (w b) :-> w (ProBase t b) )
         -> (ProBase t (w a) :-> a)
         -> (t :-> a )
proGcata (k :: forall b. Profunctor b => ProBase t (w b) :-> w (ProBase t b) )
         (g :: ProBase t (w a) :-> a)
         = g . proextract . c
  where
    c :: t :-> w (ProBase t (w a))
    c p = k $ promap v $ proProject p
    v :: t :-> w (w a)
    v p2 = produplicate $ promap g $ c p2

proParaTuple :: ( Profunctor t
           , ProRecursive t
           , Profunctor a
           )
        => (ProBase t (ProTuple t a) :-> a)
        -> (t :-> a)
proParaTuple (t :: ProBase t (ProTuple t a) :-> a) = p
  where
    p :: t :-> a
    p x = t $ promap v $ proProject x
    v :: t :-> ProTuple t a
    v x = ProTuple x (p x)

proPara :: ( Profunctor t
           , ProRecursive t
           , Profunctor a
           )
        => (forall b. (b,b) -> b)
        -> (ProBase t (PDay t a) :-> a)
        -> (t :-> a)
proPara fu (t :: ProBase t (PDay t a) :-> a) = p
  where
    p :: t :-> a
    p x = t $ promap v $ proProject x
    v :: t :-> PDay t a
    v x = PDay x (p x) fu (\c->(c,c)) -- ?????????
--    v = _a

proParaFst,proParaSnd :: ( Profunctor t
              , ProRecursive t
              , Profunctor a
              )
           => (ProBase t (PDay t a) :-> a)
           -> (t :-> a)
proParaFst = proPara fst
proParaSnd = proPara snd

proHisto :: ( Profunctor a
            , ProRecursive t
            , HPFunctor (ProBase t)
            , Profunctor t
            , Profunctor (ProBase t (ProCofree (ProBase t) a))
            )
         => (ProBase t (ProCofree (ProBase t) a) :-> a) -> (t :-> a)
proHisto = proGcata proDistHisto

{-
proHisto :: ( Profunctor a
            , ProRecursive t
            , HPFunctor (ProBase t)
            , Profunctor t
            , Profunctor (ProBase t (ProCofree (ProBase t) a))
            )
         => (ProBase t (ProCofree (ProBase t) a) :-> a) -> (t :-> a)
proHisto (f :: ProBase t (ProCofree (ProBase t) a) x y -> a x y) = (
   proGcata :: (forall b. ProBase t (ProCofree (ProBase t) b) :-> ProCofree (ProBase t) (ProBase t b) )
            -> (ProBase t (ProCofree (ProBase t) a) x y -> a x y)
            -> t x y -> a x y
  )
  (proDistHisto :: forall b. Profunctor b => ProBase t (ProCofree (ProBase t) b) x y
                -> ProCofree (ProBase t) (ProBase t b) x y
  ) f
-}

proDistHisto :: ( ProfunctorFunctor pp
                , HPFunctor pp
                , Profunctor a
                )
             => pp (ProCofree pp a) :-> ProCofree pp (pp a)
proDistHisto (fc :: pp (ProCofree pp a) x y) = ProCofree $ PProTuple
  (promap proextract fc :^  promap (proDistHisto . proUnwrap) fc)

class ProfunctorFunctor (ProBase p) => ProCorecursive p where
  proEmbed :: ProBase p p :-> p

proAna :: ( Profunctor t
          , ProCorecursive t
          , Profunctor a
          ) => (a :-> ProBase t a) -> (a :-> t)
proAna (g :: a :-> ProBase t a) = a
  where
    a :: a :-> t
    a p = proEmbed $ promap a $ g p

-- Changing representation

proHoist :: ( ProRecursive s
            , ProCorecursive t
            , Profunctor t
            , Profunctor s
            )
         => (forall a. Profunctor a => ProBase s a :-> ProBase t a) -> (s :-> t)
proHoist n = proCata (proEmbed . n)

-- ProBase

type instance ProBase (ProCofree pp p) = PProTuple p pp

instance ( ProfunctorFunctor pp
         , HPFunctor pp
         ) => ProRecursive (ProCofree pp p) where
  proProject (ProCofree (PProTuple (p :^ pp) )) = PProTuple (p :^ pp)

instance ( ProfunctorFunctor pp
         , HPFunctor pp
         ) => ProCorecursive (ProCofree pp p) where
  proEmbed = ProCofree
