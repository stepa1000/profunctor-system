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

import Data.Coerce

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
            )
         => (forall b. ProBase t (w b) :-> w (ProBase t b) )
         -> (ProBase t (w a) :-> a)
         -> (t :-> a )
proGcata (k :: forall b. ProBase t (w b) :-> w (ProBase t b) )
         (g :: ProBase t (w a) :-> a)
         = g . proextract . c
  where
    c :: t :-> w (ProBase t (w a))
    c p = k $ promap v $ proProject p
    v :: t :-> w (w a)
    v p2 = produplicate $ promap g $ c p2

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

--distHisto :: Functor f => f (Cofree f a) -> Cofree f (f a)
--distHisto fc = fmap extract fc :< fmap (distHisto . Cofree.unwrap) fc

{-}
proGpara :: ( Profunctor p
            , ProRecursive p
            , ProCorecursive p
            , ProfunctorComonad w
            ) =>
-}

--type (:-<>) p q = forall a b. (Semigroup a,Semigroup b) => p a b -> q a b
{-}
proCataMon :: ( Profunctor t
              , ProRecursive t
              ) => (FreeMon (ProBase t) :-> a) -> (t :-> a)
proCataMon (f :: FreeMon (ProBase t) :-> a) t = f $ proCata InH t
-}
{-}
proPara :: ( Profunctor t
           , ProRecursive t
           ) => (ProBase t (PDay t a) :-<> a) -> (t :-<> a)
proPara (t :: ProBase t (PDay t a) :-<> a) = p
  where
    p :: t :-<> a
    p x = t . promap (\q-> PDay q (p q) (uncurry (<>) ) (\c->(c,c)) ) $ proProject x
-}
--      where
--        g q a = PDay q a (uncurry (<>) ) (\c->(c,c))

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
