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

module Control.Profunctor.SetGet where

--import SystemGame.Generic.GetSet

--import Control.Arrow
import Control.Monad
import Control.Comonad

import Control.Monad.State.Lazy
import Control.Comonad.Store

import Control.Lens
import Control.Lens.Internal.Getter

import Data.Profunctor
import Data.Profunctor.Monad
import Data.Bifunctor.Functor

import Data.Profunctor.Strong
import Data.Profunctor.Closed
import Data.Profunctor.Choice
import Data.Profunctor.Traversing

import Data.Profunctor.Mapping
import Data.Profunctor.Yoneda
import Data.Bifunctor.Sum
import Data.Bifunctor.Product

import Data.Bifunctor.Tannen
import Data.Profunctor.Composition

import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Bifunctor

import Control.Profunctor.Object

{-}
data Method' pu o as bs = ( All SListI as
                           , All SListI bs
                           )
                           => Method'
  { setObj' :: SetterSOP' o pu as
  , getObj' :: GetterSOP o bs
  }

data Attitude' pu o as bs = ( All SListI as
                             , All SListI bs
                             )
                           => Attitude'
  { setAtt' :: SetterSOP' o pu as
  , getAtt' :: GetterSOP o bs
  , att' :: SOP Identity bs -> pu -> SOP Identity as
  }
-}

class Profunctor p => SetGet p a0 o | p -> o, p -> a0 where
  setRun :: p a b -> (a0 -> a) -> o -> o
  getRun :: p a b -> o -> b

runSetGet :: SetGet p a0 o
          => p a b -> (a0 -> a) -> o -> (b,o)
runSetGet p f = (\o->(getRun p o,o)) . setRun p f

runSet :: (SetGet p a0 o, MonadState o m)
       => p a b -> (a0 -> a) -> m ()
runSet p f = modify (setRun p f)

runGet :: (SetGet p a0 o, MonadState o m)
       => p a b -> m b
runGet p = state (\s-> (getRun p s, s))

runSG :: (SetGet p a0 o, MonadState o m)
      => p a b -> (a0 -> a) -> m b
runSG p f = runSet p f >> runGet p

runSGWpast :: (SetGet p a0 o, MonadState o m)
          => p a b -> (b -> a0 -> a) -> m b
runSGWpast p f = do
  b <- runGet p
  runSet p (f b)
  runGet p

instance SetGet (Method a0 o) a0 o where
  setRun = setMethod
  getRun = getMethod

instance ( SetGet p a0 o
         , Profunctor p
         ) => SetGet (Cotambara p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance ( SetGet p a0 o
         , Profunctor p
         ) => SetGet (Tambara p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance ( SetGet p a0 o
         , Profunctor p
         ) => SetGet (Closure p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance ( SetGet p a0 o
         , Profunctor p
         ) => SetGet (TambaraSum p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance SetGet p a0 o => SetGet (CofreeTraversing p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance SetGet p a0 o => SetGet (CofreeMapping p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance SetGet p a0 o => SetGet (Coyoneda p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance SetGet p a0 o => SetGet (Yoneda p) a0 o where
  setRun = setRun . proextract
  getRun = getRun . proextract

instance ( SetGet p a0 o
         , SetGet q a0 o
         ) => SetGet (Sum p q) a0 o where
  setRun (L2 p) = setRun p
  setRun (R2 p) = setRun p
  getRun (L2 p) = getRun p
  getRun (R2 p) = getRun p

instance ( SetGet p a0 o
         , SetGet q a0 o
         ) => SetGet (Product p q) a0 o where
  setRun = setRun . biextract
  getRun = getRun . biextract

instance ( SetGet p a0 o
         , Comonad f
         ) => SetGet (Tannen f p) a0 o where
  setRun = setRun . biextract
  getRun = getRun . biextract

instance ( SetGet p a0 o
         , SetGet q a0 o
         ) => SetGet (Procompose p q) a0 o where
  setRun (Procompose p q) f = execState $
    (StateT $ return . runSetGet q f) >>=
    (\ x -> StateT $ return . runSetGet p (const x) )
  getRun (Procompose p q) = evalState $
    (StateT $ \ s -> return (getRun q s,s) ) >>=
    (\ x -> StateT $ return . runSetGet p (const x) )

-- Monad
{-}
instance SetGet p a0 o => SetGet (Pastro p) a0 o where
  setRun (Pastro gb p ag) f o =  setRun p (ag . f) o
    where
      (x,z) = ag . f
  getRun =
-}
