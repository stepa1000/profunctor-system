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

module Data.Profunctor.Numeric.FAD where

import Control.Profunctor.Postlude

--import Data.Profunctor.System.Body

--import Data.HashMap.Lazy
--import Data.Hashable

import Data.Vector as V
import Data.Functor.Contravariant
import Data.Function

import Control.Lens
import Control.Arrow

import Numeric.FAD

--type ClassTower a = (Eq a,Num a,Enum a,Floating a
--  ,Fractional a, Ord a, Real a, RealFloat a, RealFrac a, Show a)
{-}
data HProTower p x y = forall tag a b.
    (ClassTower a, ClassTower b) => HProTower
  { runHProTower :: p (Tower tag a) (Tower tag b)
  , toHProTower :: x -> Tower tag a
  , fromHProTower :: Tower tag b -> y
  }
  | forall tag. PureHProTower ((->) (Tower tag a) (Tower tag b))
-}
--type HHProTower = FixH HProTower

--data HProTower a b = forall tag. HProTower (HProTower (Tower tag a) (Tower tag b) )
--                   | forall tag. PureHProTower ((->) (Tower tag a) (Tower tag b) )
{-}
tgF :: Fractional a => a -> a -> (a -> a) -> Maybe a
tgF x0 xEps f |xEps > 0 = if dx == 0
    then Nothing
    else return f'
  where
    dx = abs ((x0 - xEps) - (x0 + xEps) )
    dy = abs (f (x0 - xEps) - f (x0 + xEps) )
    f' = dy/dx
-}
{-}
data LinDiff a = LinDiff Int (a,a) (a,a)
               | ErrorNotEqDiff

linDiff :: Fractional a
        => LinDiff a -> LinDiff a -> LinDiff a DEBUUUUUUUUUUG MEEEEEEEEEEEEE !!!!!!!!!!!!!!!!!!!!!!!!
linDiff
-}

arrIf :: Arrow arr => arr Bool (Either () ())
arrIf = arr (\b-> if b then Right () else Left ())

-- arrIfWith :: Arrow arr => arr (Bool,a) (Either a a )

arrPreIf :: Arrow arr => arr (Predicate a, a) (Either a a)
arrPreIf = arr (\ (p,a) -> (getPredicate p a,a) ) >>>
           first arrIf >>>
           arr (\ (e,a) -> bimap (const a) (const a) e )

--          Node Leaf Leaf
dualTree2 :: (a -> a -> a -> a) -> Vector a -> a
dualTree2 f v = if V.length v > 1 && (V.length v) `mod` 2 /= 0
    then V.head $ g v
    else V.head v
  where
    g = proc v -> do
      (g <<< dutr) ||| id <<< arrPreIf -< (Predicate ((== 1) . V.length) , v)
--    dutr :: Vector a -> Vector a
    dutr va = (V.catMaybes . V.imap g) va
      where
        g i a = do
          ar <- va !? (i-1)
          al <- va !? (i+1)
          return $ f a ar al

--diffAppl :: Fractional a
--         => Vector (a,a) -> Vector (a,a) -> Vector (a,a)
--diffAppl dualTree2

tgFNV :: Fractional a
     => (a -> a) -- a -> a -> (a -> a) CoiterT x0 xEps
     -> Vector a -> a
tgFNV f (v :: Vector a) = snd $ dualTree2
    (\ (x0,_) (x1,y1) (x2,y2) -> tgD x0 x1 y1 x2 y2 ) vxy
  where
    vxy :: Vector (a,a)
    vxy = fmap (\x->(x,f x)) v
    tgD x0 xr dyr xl dyl = (x0,f'')
      where
        ddx = abs (xr - xl)
        ddy = abs (dyr - dyl)
        f'' = signum (dyr + dyl) * ddy / ddx
--    tgFN' xr xl =
--      where
--        dyr = tgF xr xEps f
--        dyl = tgF xl xEps f

toTaylorV' :: Fractional a => Int -> a -> a -> (a -> a) -> Vector a -- (HProTower a a)
toTaylorV' n x0 xEps f | n >= 1 = fmap (tgFNV f) gvv
  where
--    n2 = n*2 + 1
--    nm = n+1
    gvv = generate n gv
    gv nx = generate (nx*2 + 1) (\i->
      if i < nx + 1
        then x0 - (xEps * (fromIntegral (nx - i + 1) ))
        else if i > nx + 1
          then x0 + (xEps * (fromIntegral i ))
          else x0
      )

data Taylor a = Taylor (a,a) (Vector a) deriving Show
--                      x,y        f'n
toTaylor :: Fractional a => Int -> a -> a -> (a -> a) -> Taylor a
toTaylor n x0 xEps f = Taylor (x0,f x0) $ toTaylorV' n x0 xEps f

--taylor2Tower :: Taylor a ->  Tower tag a ->  Tower tag a
--taylor2Tower (Taylor )
