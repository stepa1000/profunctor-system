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
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Control.Profunctor.ProTuple where

import Control.Profunctor.Prelude
import Control.Profunctor.Monoidal
--import Control.Profunctor.Foldable

class ClassProTuple p where
  proBoth :: p q1 q2 a b -> (q1 a b, q2 a b)
  proFromBoth :: (q1 a b, q2 a b) -> p q1 q2 a b

pattern (:^) :: ClassProTuple p => q1 a b -> q2 a b -> p q1 q2 a b
pattern (:^) q1 q2 <- (proBoth -> (q1,q2)) where
  (:^) q1 q2 = proFromBoth (q1,q2)

data ProTuple (p :: * -> * -> *) (q :: * -> * -> *) a b = ProTuple (p a b) (q a b)
unProTuple (ProTuple a b) = (a,b)
proTuple (a,b) = ProTuple a b
instance ClassProTuple ProTuple where
  proBoth = unProTuple
  proFromBoth = proTuple

newtype PProTuple
    (p :: * -> * -> *)
    (pp :: (* -> * -> *) -> * -> * -> *)
    (q :: * -> * -> *) a b = PProTuple
  {unPProTuple :: ProTuple p (pp q) a b }

instance (Profunctor p, HPFunctor pp, Profunctor q) => Profunctor (PProTuple p pp q) where
  dimap fl fr (PProTuple (p :^ qq)) = PProTuple ((dimap fl fr p) :^  ddimap fl fr qq)

instance ProfunctorFunctor pp => ProfunctorFunctor (PProTuple p pp) where
  promap f (PProTuple (p :^ qq) ) = PProTuple (p :^  promap f qq)

data ProEither (p :: * -> * -> *) (q :: * -> * -> *) a b = ProLeft (p a b)
                                                         | ProRight (q a b)
                                                         -- Zero clear me

newtype ProZero p (pn :: (* -> * -> *) -> * -> * -> *) fp a b = ProZero
 {unProZero :: ProEither (pn fp) p a b}

instance ( Profunctor p
        , HPFunctor pn
        , Profunctor fp
        ) => Profunctor (ProZero p pn fp) where
 dimap fl fr (ProZero (ProLeft ipf)) = ProZero $ ProLeft $ ddimap fl fr ipf
 dimap fl fr (ProZero (ProRight ipf)) = ProZero $ ProRight $ dimap fl fr ipf

instance ProfunctorFunctor pn => ProfunctorFunctor (ProZero p pn) where
 promap f (ProZero (ProLeft ipf)) = ProZero $ ProLeft $ promap f ipf
 promap _ (ProZero (ProRight ipf)) = ProZero $ ProRight ipf

instance ( Profunctor p
        , ProfunctorFunctor pn
        , HPFunctor pn
        )=> HPFunctor (ProZero p pn) where
 ddimap = dimap

instance (Profunctor p, Profunctor q) => Profunctor (ProEither p q) where
 dimap fl fr (ProLeft p) = ProLeft $ dimap fl fr p
 dimap fl fr (ProRight q) = ProRight $ dimap fl fr q

instance ProfunctorFunctor (ProEither p) where
 promap f (ProRight q) = ProRight $ f q
 promap _ (ProLeft p) = ProLeft p

instance Profunctor p => HPFunctor (ProEither p) where
 ddimap = dimap
