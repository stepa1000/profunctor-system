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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Data.Comonad.Universe where

import Control.Profunctor.Postlude

import Data.Profunctor.System.Body

--import Data.HashMap.Lazy
--import Data.Hashable
--import Data.Vec.Lazy as V


--import Data.Type.Nat
--import Data.Type.Nat.LE

import Control.Lens
import Control.Comonad
import Control.Applicative

--import Data.Profunctor.Monoid.Vec

data Universe a = Universe [a] a [a]
newtype Universe2 a = Universe2 { getUniverse2 :: Universe (Universe a) }

left :: Universe a -> Universe a
left  (Universe (a:as) x bs) = Universe as a (x:bs)

right :: Universe a -> Universe a
right (Universe as x (b:bs)) = Universe (x:as) b bs

leftI,rightI :: Int -> Universe a -> Universe a
leftI 0 = id
leftI i | i >= 0 = leftI (i-1) . left
rightI 0 = id
rightI i | i >= 0 = rightI (i-1) . right

reposU :: Int -> Universe a -> Universe a
reposU 0 = id
reposU i
  | i < 0 = leftI (i * (-1))
reposU i = rightI i

reposUM :: Int -> Universe a -> Maybe (Universe a)
reposUM 0  u = return u
reposUM i (Universe as x bs)
  | and [i < 0, not $ null as] = reposUM (i+1) =<< return (Universe (tail as) (head as) (x:bs) )
  | and [i > 0, not $ null bs] = reposUM (i-1) =<< return (Universe (x:as) (head bs) (tail bs) )
reposUM _ _ = Nothing

makeUniverse fl fr x = Universe (tail $ iterate fl x) x (tail $ iterate fr x)

instance Functor Universe where
    fmap f (Universe as x bs) = Universe (fmap f as) (f x) (fmap f bs)

instance Comonad Universe where
    duplicate = makeUniverse left right
    extract (Universe _ x _) = x

instance Foldable Universe where
  foldMap f (Universe as x bs) = foldMap f as <> f x <> foldMap f bs
-- liftA3
instance Traversable Universe where
  traverse f (Universe as x bs) = liftA3 Universe (traverse f as) (f x) (traverse f bs)

type instance IxValue (Universe a) = a
type instance Index (Universe a) = Int
{-}
instance At (Universe a) where
  at i = f
    where
      f g u = (f' =<<) <$> g (extract <$> mu)
        where
          f' y = do
            (Universe as _ bs) <- mu
            reposUM (-i) (Universe as y bs)
          mu = reposUM i u
-}
takeRange :: (Int, Int) -> Universe a -> [a]
takeRange (a, b) u = take (b-a+1) x
    where Universe _ _ x
            | a < 0 = iterate left u !! (-a+1)
            | otherwise = iterate right u !! (a-1)

instance Functor Universe2 where
    fmap f = Universe2 . (fmap . fmap) f . getUniverse2

instance Comonad Universe2 where
    extract = extract . extract . getUniverse2
    duplicate = fmap Universe2 . Universe2 . shifted . shifted . getUniverse2
        where shifted :: Universe (Universe a) -> Universe (Universe (Universe a))
              shifted = makeUniverse (fmap left) (fmap right)
{-}
reposUM2 :: (Int,Int) -> Universe2 a -> Maybe (Universe2 a)
reposUM2 (0,0) = return
reposUM2 (i,_) (Universe2 uu)
  | i < 0 = reposUM2 (i + 1) =<< reposUM i uu
  | i > 0 = reposUM2 (i - 1) =<< reposUM i uu
reposUM2 (0,i) (Universe2 uu)
  | i < 0 = (reposUM2 (i + 1) . Universe2) =<< traverse (reposUM i) uu
  | i > 0 = (reposUM2 (i - 1) . Universe2) =<< traverse (reposUM i) uu
reposUM2 _ _ = Nothing
-}
type instance IxValue (Universe a) = a
type instance Index (Universe a) = Int
{-}
instance At (Universe2 a) where
  at (xi,yi) = f
    where
      f g uu = (f' =<<) <$> g (extract <$> muu)
        where
          f' y = do
            (Universe2 (Universe as (Universe aas _ bbs) bs)) <- mu
            reposUM2 (-xi,-yi) (Universe2 (Universe as (Universe aas y bbs) bs))
          muu = reposUM2 (xi,yi) uu

takeRange2 :: (Int, Int) -> (Int, Int) -> Universe2 a -> [[a]]
takeRange2 (x0, y0) (x1, y1)
    = takeRange (y0, y1)
    . fmap (takeRange (x0, x1))
    . getUniverse2
-}
{-}
data Universe nx ny a = Universe (Vec nx a) a (Vec ny a)

leftU :: Universe ('S nx) ny a -> Universe nx ('S ny) a
leftU (Universe pvl p pvr) = Universe (V.tail pvl) apvl (V.cons p pvr)
  where
    apvl = V.head pvl

rightU :: Universe nx ('S ny) a -> Universe ('S nx) ny a
rightU (Universe pvl p pvr) = Universe (V.cons p pvl) apvr (V.tail pvr)
  where
    apvr = V.head pvr

leftUTD :: ( LE nc1 nx
           , LE nc2 nx
           , SNatI nc1
           , SNatI nc2
--         , (('True) ~ (EqNat nx (Plus nc1 nc2)))
           )
        => Universe nx ny a
        -> Universe nc1 (Plus nc2 ny) a
leftUTD (Universe (pvl :: Vec nx a) p pvr) = if reflect (snat :: SNat nx) == Z
    then Universe pvl p pvr
    else Universe v2 (V.head v1) ( V.reverse (V.tail v1) V.++ (V.cons p pvr) )
  where
    (v1,v2) = split pvl
-}
{-}
data ProUniverse nx ny p x y
  = ProUniverse (ProVec nx p x y) (p x y) (ProVec ny p x y)

instance Profunctor p => Profunctor (ProUniverse nx ny p) where
  dimap fl fr (ProUniverse pvl p pvr) = ProUniverse
    (dimap fl fr pvl) (dimap fl fr p) (dimap fl fr pvr)

instance ProfunctorFunctor (ProUniverse nx ny p) where
  promap f (ProUniverse pvl p pvr) = ProUniverse
    (promap f pvl) (f p) (promap f pvr)

proULeft :: ProUniverse ('S nx) ny p x y -> ProUniverse nx ('S ny) p x y
proULeft (ProUniverse pvl p pvr) = ProUniverse (V.tail pvl) apvl (V.cons p pvr)
  where
    apvl = V.head pvl

proURight :: ProUniverse nx ('S ny) p x y -> ProUniverse ('S nx) ny p x y
proURight (ProUniverse pvl p pvr) = ProUniverse (V.cons p pvl) apvr (V.tail pvr)
  where
    apvr = V.head pvr

proULeftTD :: ( LE nc1 nx
              , LE nc2 nx
              , SNatI nc1
              , SNatI nc2
              , EqNat nx (Plus nc1 nc2)
              )
           => ProUniverse nx ny p x y
           -> ProUniverse nc1 (Plus nc2 ny) p x y
proULeftTD (ProUniverse pvl p pvr) = ProUniverse pvl p pvr

--proULeftStop :: ProUniverse nx ny p x y -> ProUniverse 'Z (Plus nx ny) p x y
--proULeftStop

newtype ProUniverseNX ny p x y nx = ProUniverseNX
  {unProUniverseNX :: ProUniverse nx ny p x y}

newtype ProUniverseNY nx p x y ny = ProUniverseNX
  {unProUniverseNX :: ProUniverse nx ny p x y}

instance ProfunctorComonad (ProUniverse nx ny p) where -- ProVecN
  proextract (ProUniverse _ e _) = e
  produplicate (ProUniverse pvl p pvr) = ProUniverse
    fi (ProUniverse pvl p pvr) ()
    where
      pul =
      pur =
      fi = induction (ProVecN empty)
-}
