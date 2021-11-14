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

module Data.Profunctor.Object where

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

data Method a0 o a b = Method
  { setObj :: Setter o o a0 a
  , getObj :: Getter o b -- (b0,b)
  }

setMethod :: Method a0 o a b -> (a0 -> a) -> o -> o
setMethod (Method s g) = over s

getMethod :: Method a0 o a b -> o -> b
getMethod (Method s g) = (^.g)

runMethod' :: Method a0 o a b -> (a0 -> a) -> o -> (b,o)
runMethod' me f = (\o->(getMethod me o,o)) . setMethod me f

runMethod :: Monad m => Method a0 o a b -> (a0 -> a) -> StateT o m b
runMethod m f = StateT (return . runMethod' m f)

instance Profunctor (Method a0 o) where
  dimap (f :: a1 -> a) (g :: c -> d) ((Method se ge) :: Method a0 o a c) = Method se' ge'
    where
      se' :: Setter o o a0 a1
      se' sf = se (fmap f . sf)
      ge' :: Getter o d  --(b0 -> Const d b0) -> s -> Const d s  -- Getting d o b0
      ge' gf s = phantom $ gf $ g $ getConst $ ge Const s
-- (b0 -> Const d b0) -> (b0 -> Const c b0)

lensToMethod :: Lens' o a -> Method a o a a
lensToMethod (le :: forall f. Functor f => (a -> f a) -> o -> f o ) = Method
  (sets (over le) :: forall f. Settable f => Optical (->) (->) f o o a a )
  (Control.Lens.to (^.le) :: forall f. Contravariant f => Optic' (->) f o a)
--  (sets (pure .  ) :: ∀ f. Settable f => ((->) a b -> (->) s t) -> Optical (->) (->) f o o a a )
--  (Control.Lens.to (^.le) :: forall f. Contravariant f => Optic' (->) f o b)

fusionGetter :: Getter o b2 -> Method a0 o a b -> Method a0 o a (b2,b)
fusionGetter ge2 (Method se ge) = Method se (\ f -> phantom . (alongside ge2 ge) f . (\x->(x,x)) )

--fusionMethod :: Setter t t a0 a -> Getter o b2
--             -> Method a0 o a b -> Method a0 (t,o) a (b2,b)
--fusionMethod se2 ge2 (Method se ge) = Method

newtype LensObj s t b a = LensObj {unLensObj :: Lens s t a b}
newtype LensObjLike f s t b a = LensObjLike {unLensObjLike :: LensLike f s t a b}

instance Profunctor (LensObj s t) where
-- (a -> b) -> (c -> d) -> Lens s t c b -> Lens s t d b
--                      LensObj s t b c
--                                      LensObj s t b d
  dimap f g (LensObj le) = LensObj $ \fx-> le (fmap f . fx . g)

mapLensObjLike :: (LensLike f s t a b -> LensLike f2 s2 t2 a2 b2)
           -> LensObjLike f s t b a -> LensObjLike f2 s2 t2 b2 a2
mapLensObjLike f = LensObjLike . f . unLensObjLike

alongsidObj :: LensLike (AlongsideLeft f b) s3 t3 a3 b3
            -> LensObjLike (AlongsideRight f t3) s t b a
            -> LensObjLike f (s3,s) (t3,t) (b3,b) (a3,a)
alongsidObj le = mapLensObjLike (alongside le)

fusionLensSS :: Lens s s a3 b3 -> Lens s s a b
                -> Lens s s (a3,a) (b3,b)
fusionLensSS (le3 :: Lens s s a3 b3) (le :: Lens s s a b) = lens
    (\s-> (s^.(le3 . (phantom .) ),s^.(le . (phantom .)) ) )
    (\ s (a3,a) -> (set le a . set le3 a3) s)

newtype ObjLens a b s t = ObjLens {unObjLens :: Lens s t a b}

instance Profunctor (ObjLens a b) where
  dimap f g (ObjLens le) = ObjLens $ \ s -> fmap g . le s . f

--fusSetLensObjSS :: Set s s a2 b2 -> LensObj s s b a

{-}
newtype Methoding a0 o a b m v = Methoding
  { unMethoding :: StateT (Method a0 o a b,o) m v
  } deriving (Monad,Applicative,Functor)

newtype Object a0 o a b w v = Object
  { unObject :: StoreT (o,Method a0 o a b) w v
  } deriving Functor

instance Comonad w => Comonad (Object a0 o a b w) where
  extract = extract . unObject
  duplicate = Object . fmap Object . duplicate . unObject

instance Comonad w => ComonadStore (o,Method a0 o a b) (Object a0 o a b w) where
  pos = pos . unObject
  peek s = peek s . unObject

upObject :: Comonad w => Object a0 o a b w o -> Object a0 o a b w ()
upObject so = void $ Object
  $ seeks (first (const $ extract so) ) $ unObject so
-- (\(so,(o,m))-> seeks (first (const $ extract so) ) so ) <<< id &&& pos
-}
data Attitude a0 o a b = Attitude
  { setAtt :: Setter o o a0 a
  , getAtt :: Getter o b
  , att :: b -> a0 -> a
  }
