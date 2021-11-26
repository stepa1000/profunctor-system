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

module Data.Profunctor.System.Arrow where

import Control.Profunctor.Postlude

import Control.Category.Free

newtype ProArrow p a b = ProArrow
  {runProArrow :: Queue (FreeSOP (Sum (->) p)) a b}

instance Category (ProArrow p a b) where
  id = ProArrow NilQ
  ProArrow a . ProArrow b = ProArrow $ a . b

instance ( Profunctor p
         , Monoidal p
         )
         => Arrow (ProArrow p a b) where
  arr bc = ProArrow $ ConsQ (L2 bc) NilQ
  first (ProArrow bc) = ProArrow $ zipWithQ (>**<) bc id
