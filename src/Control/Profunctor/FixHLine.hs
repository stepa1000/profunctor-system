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

module Control.Profunctor.FixHLine where

-- copy https://bartoszmilewski.com/2018/02/20/free-monoidal-profunctors/

import Control.Profunctor.Prelude

import Control.Profunctor.Foldable
{-}
data FixHLine pp a b = FixHLine
  { fixHLeft :: FixH pp a b
  , fixHRight :: FixH pp a b
  }
flipFixHLine (FixHLine fl fr) = FixHLine fr fl

type instance (FixHLine pp) = pp
-}
