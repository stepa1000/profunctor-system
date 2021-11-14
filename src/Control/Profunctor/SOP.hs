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
{-# LANGUAGE DataKinds #-}

module Control.Profunctor.SOP where

import Control.Profunctor.Prelude
import Control.Profunctor.Monoidal

import Generics.SOP
--import Generics.SOP.NP
--import Generics.SOP.NS
--import Generics.SOP.Constraint
--import Generics.SOP.Universe

proMultFree :: p (f x) (f y)
        -> FreeMon p (NP f xs ) (NP f ys )
        -> FreeMon p (NP f (x ': xs) ) (NP f (y ': ys) )
proMultFree p fmp = InH $ MoreP d
  where
    d = PDay p fmp (\(x,xs)->x :* xs) (\(x :* xs)->(x,xs) )
{-}
proAdd :: Maybe (p (f x) (f y) )
       -> FreeAltMon p (NS f xs) (NS f ys)
       -> FreeAltMon p (NS f (x ': xs) ) (NS f (y ': ys) )
proAdd mp famp = InH $ AltMoreP ad
  where
    ad = PAltDay
-}
proAddFree :: p (f x) (f y)
       -> FreeAllAltMon p (NS f xs) (NS f ys)
       -> FreeAllAltMon p (NS f (x ': xs) ) (NS f (y ': ys) )
proAddFree (p :: p (f x) (f y)) (famp :: FreeAllAltMon p (NS f xs) (NS f ys)) = InH $ AllAltMoreP ad
  where
    ad = PAllAltDay p famp gr gl
    gl :: NS f (x ': xs) -> Either (f x) (NS f xs)
    gl (Z x) = Left x
    gl (S ns) = Right ns
    gr :: Either (f y) (NS f ys) -> NS f (y ': ys)
    gr (Left x) = Z x
    gr (Right ns) = S ns

profunctorSOPFree :: (SListI xs, SListI ys, SListI2 xss, SListI2 yss)
              => FreeMon p (NP f xs ) (NP f ys )
              -> FreeAllAltMon (FreeMon p) (SOP f xss) (SOP f yss)
              -> FreeAllAltMon (FreeMon p) (SOP f (xs ': xss) ) (SOP f (ys ': yss) )
profunctorSOPFree (fmp :: FreeMon p (NP f xs ) (NP f ys ) )
              (faamfmp :: FreeAllAltMon (FreeMon p) (SOP f xss) (SOP f yss) )
              = InH $ AllAltMoreP ad
  where
    ad = PAllAltDay fmp faamfmp gr gl
    gl :: SOP f (xs ': xss) -> Either (NP f xs) (SOP f xss)
    gl (SOP (Z x)) = Left x
    gl (SOP (S ns)) = Right (SOP ns)
    gr :: Either (NP f ys) (SOP f yss) -> SOP f (ys ': yss)
    gr (Left x) = SOP $ Z x
    gr (Right (SOP ns) ) = SOP $ S ns

profunctorGenericFree :: ( Generic a
                     , Generic b
                     , Rep a ~ SOP I xss
                     , Rep b ~ SOP I yss
                     )
                  => FreeAllAltMon (FreeMon p) (SOP I xss) (SOP I yss)
                  -> FreeAllAltMon (FreeMon p) a b
profunctorGenericFree = dimap from to
